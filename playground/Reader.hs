{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase    #-}
module Main where

import Prelude hiding (read, readFile, writeFile)
import Data.BitCode
import Data.BitCode.Reader.Monad
import Data.BitCode.Reader.Combinators
import Data.BitCode.Reader
import Data.BitCode.Writer.Monad (writeFile)
import Data.BitCode.Writer (emitTopLevel)
import Data.BitCode.Writer.Combinators
import System.Environment (getArgs)
import Debug.Trace (traceM)


parseFile :: FilePath -> IO ()
parseFile f = readFile f >>= \case
  Left err -> putStrLn "== Error ==" >> putStrLn err
  Right r  -> print r

readWriteFile :: FilePath -> IO ()
readWriteFile f = readFile f >>= \case
  Left err -> putStrLn "** Error " >> putStrLn err
  Right r  -> putStrLn (show r) >> writeFile' f' r
  where f' = f ++ ".out"
        writeFile' :: FilePath -> [Block] -> IO ()
        writeFile' fp = writeFile fp . withHeader True . emitTopLevel

main :: IO ()
main = getArgs >>= \case
  [] -> putStrLn "expecting a bitcode file as argument"
  [f] -> parseFile f
