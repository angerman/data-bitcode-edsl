{-# LANGUAGE NoImplicitPrelude #-}

module EDSL.Example.HelloWorld where
import Prelude hiding (mod)
import EDSL

helloWorld = mod "helloWorld"
  [ def_ "square" ([i32] --> i32) $ \[ arg0 ] -> do
      block "entry" $ do
        ret =<< arg0 `mul` arg0
  , def_ "main" ([i32, ptr i8ptr] --> i32) $ \[ argc, argv ] -> do
      block "entry" $ do
        strPtr  <- gep (global "foo" (cStr "hello world, %d\n")) [int32 0, int32 0]
        Just sq <- ccall (fun "square" ([i32] --> i32)) [int32 3]
        ccall (fun "printf" (vararg $ [i8ptr] --> i32)) [strPtr, sq]
        ret $ int32 0
  ]
