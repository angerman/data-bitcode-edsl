{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecursiveDo       #-}
module EDSL.Example.HelloBranch where

import Prelude hiding (mod)
import EDSL
import EDSL.Monad.EdslT

import Text.PrettyPrint (Doc)
import Data.BitCode.LLVM.Pretty
import Data.BitCode.LLVM.Classes.HasType (ty)
import Data.Maybe
import Data.BitCode.LLVM (Module)

import Data.BitCode.LLVM.Types (BasicBlockId)
-- import EDSL.Monad (BodyBuilder)

-- reexport pretty (to make intero happy :-))
pp :: (Pretty a) => a -> Doc
pp = pretty

helloBranch, helloBranch2, helloPtrFn, helloWorld, gepFun, binOpFun, prefixDataFun :: Module

helloBranch = mod "helloWorld"
  [ def "main" ([i32, ptr =<< i8ptr] --> i32) $ \[ argc, _argv ] -> mdo
      block "entry" $ do
        constOne <- int32 1
        cond <- argc `iugt` constOne 
        br cond one two
      one <- block "one" $ do
        strPtr <- bind2 gep (global "one" =<< cStr "One") (sequence [int32 0, int32 0])
        puts <- fun "puts" =<< [i8ptr] --> i32
        ccall puts [strPtr]
        ret =<< int32 0
      two <- block "two" $ do
        two <- global "two" =<< cStr "Two" 
        strPtr <- gep two =<< sequence [int32 0, int32 0]
        f <- fun "puts" =<< [i8ptr] --> i32
        ccall f [strPtr]
        ret =<< int32 0
      pure ()
  ]

lookup_ k = fromMaybe (fail "not found") . fmap pure . lookup k

genBlock :: [(String, BasicBlockId)] -> String -> Edsl (String, BasicBlockId)
genBlock idMap str = block' str $ do
  g <- global str =<< cStr str 
  strPtr <- gep g =<< sequence [int32 0, int32 0]
  puts <- fun "puts" =<< [i8ptr] --> i32
  ccall puts [strPtr]
  ubr =<< lookup_ "end" idMap


helloBranch2 = mod "helloWorld"
  [ def "main" ([i32, ptr =<< i8ptr] --> i32) $ \[ argc, argv ] -> mdo
      idMap <- (++) <$> (mapM (genBlock idMap) ["foo", "bar", "baz"]) <*> ((:[]) <$> (block' "end" (ret =<< int32 0)))
      pure ()
  ]

helloPtrFn = mod "helloWorld"
  [ def "main" ([i32, ptr =<< i8ptr] --> i32) $ \[ argc, argv ] -> mdo
      let sig = [i8ptr] --> i32
      block "entry" $ do
        -- obtain a slot to store an int32
        intSlot <- bind2 alloca i32 (int32 1)
        -- store said int32
        store intSlot =<< int32 2

        ptrSlot <- bind2 alloca i64 (int32 1)
        ptr' <- bind2 ptrToInt i64 (pure intSlot)
        store ptrSlot ptr'
        intSlot' <- bind2 intToPtr (ptr =<< i64) (load ptrSlot)
        n <- load intSlot'
        foo <- global "foo" =<< cStr "n: %d\n" 
        strPtr <- gep foo =<< sequence [int32 0, int32 0]
        printf <- fun "printf" =<< (vararg $ [i8ptr] --> i32) 
        ccall printf [strPtr, n]
        -- get a slot to store a function of (i8ptr -> i32) (e.g. the ptr to something of that type.)
        slot <- bind2 alloca (ptr =<< sig) (int32 1)
        -- store puts in there.
        store slot =<< fun "puts" =<< sig
        -- load puts.
        f <- load slot
        -- call puts.
        foo' <- global "foo" =<< cStr "hello world\n"
        strPtr <- gep foo' =<< sequence [int32 0, int32 0]
        puts <- fun "puts" =<< sig
        ccall puts [strPtr]
        ccall f    [strPtr]
        -- return 0
        ret =<< int32 0
  ]

helloWorld = mod "helloWorld"
  [ def "main" ([i32, ptr =<< i8ptr] --> i32) $ \[ argc, argv ] -> do
      block "entry" $ do
        foo <- global "foo" =<< cStr "hello world\n" 
        strPtr <- gep foo =<< sequence [int32 0, int32 0]
        printf <- fun "printf" =<< (vararg $ [i8ptr] --> i32)
        ccall printf [strPtr]
        ret =<< int32 0
  ]

gepFun = mod "helloWorld"
  [ def "main" ([i32, ptr =<< i8ptr] --> i32) $ \[ argc, argv ] -> do
      block "entry" $ do
        p <- gep argv =<< sequence [int32 0]
        foo <- global "foo" =<< cStr "hello world\n" 
        strPtr <- gep foo =<< sequence [int32 0, int32 0]
        ret $ p
  ]

binOpFun = mod "helloWorld"
  [ def "main" ([i32, ptr =<< i8ptr] --> i32) $ \[ argc, argv ] -> do
      block "entry" $ do
        slot <- bind2 alloca i32 (int32 1)
        store slot =<< int32 4
        ret =<< sub slot =<< int32 2
  ]

prefixDataFun = mod "prefixData"
  [ withPrefixDataM prefix $
    def "main" ([i32, ptr =<< i8ptr] --> i32) $ \[ argc, argv ] -> do
      block "entry" $ do
        -- obtain a pointer to the function
        -- prefixTy <- ty <$> prefix
        fp <- bind2 bitcast (ptr =<< ty <$> prefix) (fun "main" =<< [i32, ptr =<< i8ptr] --> i32)
        -- get the pointer to the data; and load it.
        v <- load =<< gep fp =<< sequence [int32 (-1), int32 1]
        str <- global "str" =<< cStr "prefix data value: %d\n" 
        strPtr <- gep str =<< sequence [int32 0, int32 0]
        printf <- fun "printf" =<< (vararg $ [i8ptr] --> i32) 
        ccall printf [strPtr, v]
        ret =<< int32 0
  ]
  where prefix = struct =<< sequence [(int32 1), (int32 10)]

testWrite, testWrite2 :: IO ()
testWrite = writeModule "helloPtrFn.bc" helloPtrFn
testWrite2 = writeModule "binOpFun.bc" binOpFun
