{-# LANGUAGE TupleSections #-}
module Data.BitCode.Writer where

import Data.BitCode
import Data.BitCode.Writer.Monad as M
import Data.BitCode.Writer.ToBitCode

writeFile :: FilePath -> [Block] -> IO ()
writeFile fp = M.writeFile fp . emitTopLevel

emitTopLevel :: [Block] -> BitCodeWriter ()
emitTopLevel = mapM_ (emit . (2::Int,))
