{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase    #-}
module Data.BitCode.PlaygroundReader where

import Prelude hiding (read, readFile)
import Control.Applicative ((<|>), some)
import Data.BitCode.Reader
import Data.BitCode.AST
import Data.BitCode.IDs.FixedAbbrev

import Control.Monad (replicateM)

import Data.Word (Word8, Word32, Word64)
import Data.Bits (FiniteBits, setBit, zeroBits, shift, (.|.))

import System.Environment (getArgs)

parseBytes :: [Word8] -> BitCodeReader ()
parseBytes ws = do
  bits' <- read len
  if bits == bits' then return ()
    else fail "no parse"
  where bits :: Bits
        bits = concatMap fromByte ws
        len  = length bits

toFiniteBits :: (FiniteBits a) => Bits -> a
toFiniteBits = foldl setBit zeroBits . map fst . filter ((== True) . snd) . zip [0..]

parseFixed :: (FiniteBits a) => Int -> BitCodeReader a
parseFixed n = read n >>= return . toFiniteBits

parseVBR :: (FiniteBits a) => Int -> BitCodeReader a
parseVBR n = do
  v <- parseFixed (n-1)
  c <- read 1
  if c == [False]
    then return v
    else do v' <- parseVBR n
            return $ v .|. (shift v' (n-1))

readFixed :: (Show a, FiniteBits a) => Int -> a -> BitCodeReader ()
readFixed n e = do
  v <- parseFixed n
  if e == v then return () else fail $ "failed to read " ++ show e ++ "; got " ++ show v

readBit :: Bool -> BitCodeReader ()
readBit = readFixed 1

parseWord32 :: BitCodeReader Word32
parseWord32 = parseFixed 32

parseHeader :: BitCodeReader (Word32, Word32, Word32, Word32)
parseHeader = do
  parseBytes [0xde,0xc0,0x17,0x0b]
  version <- parseWord32
  offset  <- parseWord32
  len     <- parseWord32
  cpuType <- parseWord32
  return (version, offset, len, cpuType)
--  (,,,) <$> parseWord32 <*> parseWord32 <*> parseWord32 <*> parseWord32

skipToNbits :: Int -> BitCodeReader ()
skipToNbits n = snd <$> ask >>= \x -> read ((32 + n - x) `mod` 32) >> return ()
skipTo32bits :: BitCodeReader ()
skipTo32bits = skipToNbits 32

position :: BitCodeReader Int
position = snd <$> ask

parseLLVMIRHeader :: BitCodeReader ()
parseLLVMIRHeader = parseBytes [0x42,0x43,0xc0,0xde]

parseSubBlock :: Int -> BitCodeReader Block
parseSubBlock width = do
  pos      <- position
  readFixed width (fromEnum ENTER_SUBBLOCK)
  id       <- parseVBR 8
  newWidth <- parseVBR 4
--  if pos == 0
--    then skipTo32bits
--    else skipToNbits pos
  skipTo32bits
  len      <- parseWord32
  blocks   <- parseStream newWidth
  readFixed width (fromEnum END_BLOCK)
--  if pos == 0
--    then skipTo32bits
--    else skipToNbits pos
  skipTo32bits
  return $ Block id newWidth blocks

parseUnabbrevRecord :: Int -> BitCodeReader Block
parseUnabbrevRecord width = do
  readFixed width (fromEnum UNABBREV_RECORD)
  code     <- parseVBR 6
  len      <- parseVBR 6
  ops      <- parseOps len
  return $ UnabbrevRecord code ops
  where parseOps :: Int -> BitCodeReader [Word64]
        parseOps 1 = pure <$> parseVBR 6
        parseOps n = (:) <$> parseVBR 6 <*> parseOps (n-1)

parseEncVal :: BitCodeReader EncVal
parseEncVal = parseFixedVal <|> parseVBRVal <|> parseArr <|> parseChar6 <|> parseBlob
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

parseOp :: BitCodeReader Op
parseOp = parseLit <|> parseEnc
  where parseLit = Lit <$> (readBit True *> parseVBR 8)
        parseEnc = Enc <$> (readBit False *> parseEncVal)

parseChar6 :: BitCodeReader Char
parseChar6 = decodeChar6 <$> parseFixed 6
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
                                      return (concat fields, xs)
parseEncField (Enc Char6:xs)     = (,xs) . pure . Chr <$> parseChar6

parseDefAbbrevRecord :: Int -> BitCodeReader Block
parseDefAbbrevRecord width = do
  readFixed width (fromEnum DEFINE_ABBREV)
  len      <- parseVBR 5
  ops      <- parseOps len
  return $ DefAbbrevRecord ops
  where parseOps :: Int -> BitCodeReader [Op]
        parseOps 1 = pure <$> parseOp
        parseOps n = (:) <$> parseOp <*> parseOps (n-1)


type Code = Int
newtype AbbrevMap = AbbrevMap [(Code, Block)]

instance Monoid AbbrevMap where
  mempty = AbbrevMap []
  (AbbrevMap m) `mappend` (AbbrevMap n) = AbbrevMap (m ++ n)

lookupAbbrev :: AbbrevMap -> Code -> Maybe Block
lookupAbbrev (AbbrevMap m) = flip lookup m

addAbbrev :: AbbrevMap -> Block -> AbbrevMap
addAbbrev (AbbrevMap m) r@(DefAbbrevRecord ops) = AbbrevMap $ (nextId,r):m
  where nextId = 1 + foldr max 3 (map fst m)

parseAbbrevRecord :: Int -> AbbrevMap -> BitCodeReader Block
parseAbbrevRecord width abbrevs = do
  code   <- parseFixed width
  let abbrev = lookupAbbrev abbrevs code
  case abbrev of
    Nothing -> fail $ "No record for the given abbreviation code: " ++ show code
    Just (DefAbbrevRecord ops) -> AbbrevRecord code <$> parseAbbrevRecord' ops
  where parseAbbrevRecord' :: [Op] -> BitCodeReader [Field]
        parseAbbrevRecord' ops = do (flds, ops') <- parseEncField ops
                                    case ops' of
                                      [] -> return flds
                                      _  -> (flds++) <$> parseAbbrevRecord' ops'

parseBlock :: Int -> AbbrevMap -> BitCodeReader Block
parseBlock n abbrevs = parseSubBlock n <|> parseUnabbrevRecord n <|> parseDefAbbrevRecord n <|> parseAbbrevRecord n abbrevs

parseStream :: Int -> BitCodeReader [Block]
parseStream n = go n mempty
  where go :: Int -> AbbrevMap -> BitCodeReader [Block]
        go n abbrevs = (Just <$> parseBlock n abbrevs <|> pure Nothing) >>= \case
          Nothing -> return []
          Just r@(DefAbbrevRecord ops) -> (r:) <$> go n (addAbbrev abbrevs r)
          Just r -> (r:) <$> go n abbrevs

testParse :: IO ()
testParse = do
  bits <- readFile "test.bc"
  let res = evalBitCodeReader bits $ do
                parseHeader
                parseLLVMIRHeader
                parseStream 2
                -- parseSubBlock 3
                -- parseBlock 2 mempty
                -- ppBitCode

  case res of
    Left err -> putStrLn "== Error ==" >> putStrLn err
    Right r  -> print r

main :: IO ()
main = getArgs >>= print
