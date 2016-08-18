{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecursiveDo       #-}
module EDSL.Example.HelloBranch where

import Prelude hiding (mod)
import EDSL

import Text.PrettyPrint (Doc)
import Data.BitCode.LLVM.Pretty
import Data.BitCode.LLVM.Classes.HasType (ty)
import Data.Maybe

import Data.BitCode.LLVM.Types (BasicBlockId)
import Data.BitCode.LLVM.Util (lift)
import EDSL.Monad (BodyBuilder)

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
        ccall (fun "puts" ([i8ptr] --> i32)) [strPtr]
        ret $ int32 0
      two <- block "two" $ do
        strPtr <- gep (global "two" (cStr "Two")) [int32 0, int32 0]
        ccall (fun "puts" ([i8ptr] --> i32)) [strPtr]
        ret $ int32 0
      pure ()
  ]

lookup_ k = fromMaybe (fail "not found") . fmap pure . lookup k

genBlock :: [(String, BasicBlockId)] -> String -> BodyBuilder (String, BasicBlockId)
genBlock idMap str = block' str $ do
  strPtr <- gep (global str (cStr str)) [int32 0, int32 0]
  ccall (fun "puts" ([i8ptr] --> i32)) [strPtr]
  ubr =<< lookup_ "end" idMap


helloBranch2 = mod "helloWorld"
  [ def "main" ([i32, ptr i8ptr] --> i32) $ \[ argc, argv ] -> mdo
      idMap <- (++) <$> (mapM (genBlock idMap) ["foo", "bar", "baz"]) <*> ((:[]) <$> (block' "end" (ret (int32 0))))
      pure ()
  ]

helloPtrFn = mod "helloWorld"
  [ def "main" ([i32, ptr i8ptr] --> i32) $ \[ argc, argv ] -> mdo
      let sig = [i8ptr] --> i32
      block "entry" $ do
        -- obtain a slot to store an int32
        intSlot <- alloca i32 (int32 1)
        -- store said int32
        store intSlot (int32 2)

        ptrSlot <- alloca i64 (int32 1)
        ptr <- ptrToInt i64 intSlot
        store ptrSlot ptr
        ptr' <- load ptrSlot
        intSlot' <- intToPtr (lift i64) ptr'
        n <- load intSlot'
        strPtr <- gep (global "foo" (cStr "n: %d\n")) [int32 0, int32 0]
        ccall (fun "printf" (vararg $ [i8ptr] --> i32)) [strPtr, n]
        -- get a slot to store a function of (i8ptr -> i32) (e.g. the ptr to something of that type.)
        slot <- alloca (lift sig) (int32 1)
        -- store puts in there.
        store slot (fun "puts" sig)
        -- load puts.
        f <- load slot
        -- call puts.
        strPtr <- gep (global "foo" (cStr "hello world\n")) [int32 0, int32 0]
        ccall (fun "puts" sig) [strPtr]
        ccall f                [strPtr]
        -- return 0
        ret $ int32 0
  ]

helloWorld = mod "helloWorld"
  [ def "main" ([i32, ptr i8ptr] --> i32) $ \[ argc, argv ] -> do
      block "entry" $ do
        strPtr <- gep (global "foo" (cStr "hello world\n")) [int32 0, int32 0]
        ccall (fun "printf" (vararg $ [i8ptr] --> i32)) [strPtr]
        ret $ int32 0
  ]

gepFun = mod "helloWorld"
  [ def "main" ([i32, ptr i8ptr] --> i32) $ \[ argc, argv ] -> do
      block "entry" $ do
        p <- gep argv [int32 0]
        strPtr <- gep (global "foo" (cStr "hello world\n")) [int32 0, int32 0]
        ret $ p
  ]

binOpFun = mod "helloWorld"
  [ def "main" ([i32, ptr i8ptr] --> i32) $ \[ argc, argv ] -> do
      block "entry" $ do
        slot <- alloca i32 (int32 1)
        store slot (int32 4)
        ret =<< sub slot (int32 2)
  ]

prefixDataFun = mod "prefixData"
  [ withPrefixData prefix $
    def "main" ([i32, ptr i8ptr] --> i32) $ \[ argc, argv ] -> do
      block "entry" $ do
        -- obtain a pointer to the function
        fp <- bitcast (ptr (ty prefix)) (fun "main" ([i32, ptr i8ptr] --> i32))
        -- get the pointer to the data; and load it.
        v <- gep fp [int32 (-1), int32 1] >>= load
        strPtr <- gep (global "str" (cStr "prefix data value: %d\n")) [int32 0, int32 0]
        ccall (fun "printf" (vararg $ [i8ptr] --> i32)) [strPtr, v]
        ret (int32 0)
  ]
  where prefix = struct [(int32 1), (int32 10)]

testWrite = writeModule "helloPtrFn.bc" helloPtrFn
testWrite2 = writeModule "binOpFun.bc" binOpFun
