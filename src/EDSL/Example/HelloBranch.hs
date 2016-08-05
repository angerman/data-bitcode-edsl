{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecursiveDo       #-}
module EDSL.Example.HelloBranch where

import Prelude hiding (mod)
import EDSL

import Text.PrettyPrint (Doc)
import Data.BitCode.LLVM.Pretty

-- reexport pretty (to make intero happy :-))
pp :: (Pretty a) => a -> Doc
pp = pretty


helloBranch = mod "helloWorld"
  [ def "main" ([i32, ptr i8ptr] --> i32) $ \[ argc, argv ] -> mdo
      block "entry" $ do
        cond <- argc `iugt` int32 1
        br cond one two
      one <- block "one" $ do
        strPtr <- gep (global "one" (cStr "One")) [int32 0, int32 0]
        call (fun "puts" ([i8ptr] --> i32)) [strPtr]
        ret $ int32 0
      two <- block "two" $ do
        strPtr <- gep (global "two" (cStr "Two")) [int32 0, int32 0]
        call (fun "puts" ([i8ptr] --> i32)) [strPtr]
        ret $ int32 0
      pure ()
  ]
