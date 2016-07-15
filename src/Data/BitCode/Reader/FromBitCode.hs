{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase        #-}
module Data.BitCode.Reader.FromBitCode where

import Data.Word (Word8, Word64)
import Control.Applicative ((<|>))
import Control.Monad (replicateM)

import Data.BitCode
import Data.BitCode.Abbreviation

import Data.BitCode.Reader.Monad
import Data.BitCode.Reader.Combinators

import Data.BitCode.IDs.FixedAbbrev


-- * FromBitCode instances
instance FromBitCode EncVal where
  parse = parseFixedVal <|> parseVBRVal <|> parseArr <|> parseChar6 <|> parseBlob
    where parseFixedVal :: BitCodeReader EncVal
          parseFixedVal = Fixed <$> ((readFixed 3 (1 :: Word8)) *> parseVBR 5)
          parseVBRVal   :: BitCodeReader EncVal
          parseVBRVal   = VBR <$> ((readFixed 3 (2 :: Word8)) *> parseVBR 5)
          parseArr      :: BitCodeReader EncVal
          parseArr      = readFixed 3 (3 :: Word8) >> pure Arr
          parseChar6    :: BitCodeReader EncVal
          parseChar6    = readFixed 3 (4 :: Word8) >> pure Char6
          parseBlob     :: BitCodeReader EncVal
          parseBlob     = readFixed 3 (5 :: Word8) >> pure Blob

instance FromBitCode Op where
  parse = parseLit <|> parseEnc
    where parseLit = Lit <$> (readBit True *> parseVBR 8)
          parseEnc = Enc <$> (readBit False *> parse)

instance FromBitCode Char where
  parse = decodeChar6 <$> parseFixed 6
    where decodeChar6 :: Int -> Char
          decodeChar6 63 = '_'
          decodeChar6 62 = '.'
          decodeChar6 c | 0  <= c && c < 26 = toEnum $ c + fromEnum 'a'
          decodeChar6 c | 26 <= c && c < 52 = toEnum $ c - 26 + fromEnum 'A'
          decodeChar6 c | 52 <= c && c < 61 = toEnum $ c - 52 + fromEnum '0'

parseEncField :: [Op] -> BitCodeReader ([Field],[Op])
parseEncField (Lit v:xs)         = (,xs) . pure <$> pure (W64 v)
parseEncField (Enc (Fixed n):xs) = (,xs) . pure . Fix (fromIntegral n) <$> parseFixed (fromIntegral n)
parseEncField (Enc (VBR n):xs)   = (,xs) . pure . Vbr (fromIntegral n) <$> parseVBR (fromIntegral n)
parseEncField (Enc Arr:op:xs)    = do len <- parseVBR 6
                                      fields <- replicateM len (fst <$> parseEncField [op])
                                      return ((Vbr 6 (fromIntegral len)):concat fields, xs)
parseEncField (Enc Char6:xs)     = (,xs) . pure . Chr <$> parse

parseBlock :: Int -> AbbrevMap -> BitCodeReader Block
parseBlock n abbrevs = parseSubBlock n <|> parseUnabbrevRecord n <|> parseDefAbbrevRecord n <|> parseAbbrevRecord n abbrevs
  where parseSubBlock :: Int -> BitCodeReader Block
        parseSubBlock width = do
          readFixed width (fromEnum ENTER_SUBBLOCK)
          id       <- parseVBR 8
          newWidth <- parseVBR 4
          skipTo32bits
          len      <- parseWord32
          blocks   <- askGlobalAbbrevs id >>= parseStream newWidth
          readFixed newWidth (fromEnum END_BLOCK)
          skipTo32bits
          if id == 0 then processBlockInfo blocks else return ()
          return $ Block id newWidth blocks
            where processBlockInfo :: [Block] -> BitCodeReader ()
                  processBlockInfo = go 0
                    where go :: Int -> [Block] -> BitCodeReader ()
                          go _ [] = pure ()
                          go _ ((UnabbrevRecord 1 [id]):bs) = go (fromIntegral id) bs
                          go id (r@(DefAbbrevRecord _):bs) = tellGlobalAbbrev id r >> go id bs
                          go id (b:bs) = fail $ "*** Can not handle block: " ++ show b
        parseUnabbrevRecord :: Int -> BitCodeReader Block
        parseUnabbrevRecord width = do
          readFixed width (fromEnum UNABBREV_RECORD)
          code     <- parseVBR 6
          len      <- parseVBR 6
          ops      <- parseOps len
          return $ UnabbrevRecord code ops
            where parseOps :: Int -> BitCodeReader [Word64]
                  parseOps 0 = pure []
                  parseOps 1 = pure <$> parseVBR 6
                  parseOps n = (:) <$> parseVBR 6 <*> parseOps (n-1)
        parseDefAbbrevRecord :: Int -> BitCodeReader Block
        parseDefAbbrevRecord width = do
          readFixed width (fromEnum DEFINE_ABBREV)
          len      <- parseVBR 5
          ops      <- parseOps len
          return $ DefAbbrevRecord $! ops
            where parseOps :: Int -> BitCodeReader [Op]
                  parseOps 1 = pure <$> parse
                  parseOps n = (:) <$> parse <*> parseOps (n-1)
        parseAbbrevRecord :: Int -> AbbrevMap -> BitCodeReader Block
        parseAbbrevRecord width abbrevs = do
          code   <- parseFixed width
          if code < 4
            then fail $ "Not an abbrev code: " ++ show code
            else return ()
          let abbrev = lookupAbbrev abbrevs code
          fields <- case abbrev of
            Nothing -> fail $ "No record for the given abbreviation code: " ++ show code
            Just (DefAbbrevRecord ops) -> parseAbbrevRecord' ops
          return $ AbbrevRecord code fields
            where parseAbbrevRecord' :: [Op] -> BitCodeReader [Field]
                  parseAbbrevRecord' ops = do (flds, ops') <- parseEncField ops
                                              case ops' of
                                                [] -> return flds
                                                _  -> (flds++) <$> parseAbbrevRecord' ops'

parseStream :: Int -> AbbrevMap -> BitCodeReader [Block]
parseStream = go
  where go :: Int -> AbbrevMap -> BitCodeReader [Block]
        go n abbrevs = do
          (Just <$> parseBlock n abbrevs <|> pure Nothing) >>= \case
            Nothing -> return []
            Just r@(DefAbbrevRecord ops) -> (r:) <$> go n (addAbbrev abbrevs r)
            Just r -> (r:) <$> go n abbrevs
