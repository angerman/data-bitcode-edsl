{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-unused-matches #-}

module EDSL.Example.HelloWorld where
import Prelude hiding (mod)
import EDSL
import Data.BitCode.LLVM (Module)

import Data.BitCode.LLVM.Value (external)

helloWorld :: Module
helloWorld = mod "helloWorld"
  [ def "square" ([i32] --> i32) $ \[ arg0 ] -> do
      block "entry" $ do
        ret =<< arg0 `mul` arg0
  , def "main" ([i32, ptr =<< i8ptr] --> i32) $ \[ argc, argv ] -> do
      block "entry" $ do
        foo <- global external "foo" =<< (cStr "hello world, %d\n")
        strPtr  <- gep foo =<< sequence [int32 0, int32 0]
        square <- fun "square" =<< [i32] --> i32
        Just sq <- ccall square =<< sequence [int32 3]
        printf <- fun "printf" =<< (vararg $ [i8ptr] --> i32)
        ccall printf [strPtr, sq]
        ret =<< int32 0
  ]
