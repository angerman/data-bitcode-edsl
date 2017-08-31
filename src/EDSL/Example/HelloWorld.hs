{-# LANGUAGE NoImplicitPrelude #-}

module EDSL.Example.HelloWorld where
import Prelude hiding (mod)
import EDSL
import EDSL.Monad.Types
import EDSL.Monad.Instructions
import EDSL.Monad.Instructions.Binary

import Data.BitCode.LLVM.Value (external)

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
        f <- fun "printf" =<< (vararg $ [i8ptr] --> i32)
        ccall f [strPtr, sq]
        ret =<< int32 0
  ]
