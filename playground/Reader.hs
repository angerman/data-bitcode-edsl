{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase    #-}
module Main where

import Prelude hiding (read, readFile, writeFile)
import Data.BitCode
import Data.BitCode.LLVM
import Data.BitCode.LLVM.Reader.Monad
import Data.BitCode.LLVM.FromBitCode hiding (BitCode)
import Data.BitCode.Reader.Monad
import Data.BitCode.Reader.Combinators
import Data.BitCode.Reader
import Data.BitCode.Writer.Monad (writeFile)
import Data.BitCode.Writer (emitTopLevel)
import Data.BitCode.Writer.Combinators
import System.Environment (getArgs)
import Debug.Trace (traceM)

import Data.Maybe (catMaybes)
import Control.Monad (liftM)

import Annotated.HelloWorld

import Data.BitCode.LLVM.Pretty
import Text.PrettyPrint (render, ($+$))

import Data.BitCode.LLVM.ToBitCode
import Data.BitCode.LLVM.Codes.Identification (Epoch(Current))

import Data.BitCode.LLVM.Value (Symbol)
import Data.BitCode.LLVM.Type  (Ty, subTypes)
import Data.BitCode.LLVM.Classes.ToSymbols

import Data.List (sort, nub)

parseFile :: FilePath -> IO ()
parseFile f = readFile f >>= \case
  Left err -> putStrLn "== Error ==" >> putStrLn err
  Right r  -> print r

readWriteFile :: FilePath -> IO ()
readWriteFile f = readFile f >>= \case
  Left err -> putStrLn "** Error " >> putStrLn err
  Right r  -> putStrLn (show r) >> writeFile' f' r
  where f' = f ++ ".out"

writeFile' :: FilePath -> [BitCode] -> IO ()
writeFile' fp = writeFile fp . withHeader True . emitTopLevel

main :: IO ()
main = getArgs >>= \case
  [] -> putStrLn "expecting a bitcode file as argument"
  [f] -> parseFile f

testParseBitCode = parseFile "data/hello.bc"
testNormalize = catMaybes . map normalize $ helloWorld
test = evalLLVMReader (parseTopLevel (catMaybes . map normalize $ helloWorld))

testParse :: FilePath -> IO (Either String (Maybe Ident, Module))
testParse f = do
  res <- readFile f
  return $ (evalLLVMReader . parseTopLevel . catMaybes . map normalize) =<< res

testPretty :: FilePath -> IO ()
testPretty f = testParse f >>= \case
    Left err -> putStrLn $ "Error: " ++ err
    Right (Just i, m) -> putStrLn $ render (pretty i $+$ pretty m)
    Right (Nothing, m) -> putStrLn $ render (pretty m)

testToSymbols :: FilePath -> IO [Ty]
testToSymbols f = testParse f >>= \case
  Left err -> fail err
  Right (_, m) -> let ts = nub . sort $ map ty $ symbols m
                  in return $ nub . sort $ ts ++ concatMap subTypes ts

testToNBitCode :: [NBitCode]
testToNBitCode = concat [toBitCode (Ident "TinyBitCode" Current)
                        ,toBitCode (Module 1 Nothing Nothing [] [] [])
                        ]

testToBitCode :: [BitCode]
testToBitCode = map denormalize $ testToNBitCode

testWriteNBitCode :: IO ()
testWriteNBitCode = writeFile' "data/out.bc" testToBitCode

testToBitCodeFromFile :: FilePath -> IO [BitCode]
testToBitCodeFromFile f = testParse f >>= \case
  Left err -> fail err
  Right (_, m) -> return $ map denormalize $ toBitCode m
