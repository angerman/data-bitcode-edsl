{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecursiveDo       #-}

module EDSLSpec where

import Test.Tasty.Hspec

import Prelude hiding (mod)
import EDSL
import EDSL.Monad.EdslT
import Data.BitCode.LLVM.Value (Symbol)
import EDSL.Monad.Instructions

import Data.BitCode.LLVM (Module)
import Data.BitCode.LLVM.Classes.HasType (ty)

import Data.BitCode.LLVM.Pretty

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((-<.>), (</>))
import GHC.Stack (HasCallStack)
import System.Exit (ExitCode(..))

compile :: HasCallStack => FilePath -> IO FilePath
compile f = do
  (exit, _out, _err) <- readProcessWithExitCode
                        "clang"
                        [ "-w" -- no warnings
                        , f
                        , "-o"
                        , fout ]
                        "" --stdin
  case exit of
    ExitSuccess -> return fout
    err         -> error $ show err
  where
    fout = f -<.> "exe" -- yuck!

run :: HasCallStack => FilePath -> [String] -> IO (Int, String, String)
run f args = do
  (exit,out,err) <- readProcessWithExitCode
                    f args ""
  case exit of
    ExitSuccess     -> return (0, out, err)
    (ExitFailure n) -> return (n, out, err) 

decompile :: HasCallStack => FilePath -> IO FilePath
decompile f = do
  (exit, _out, _err) <- readProcessWithExitCode
                        "llvm-dis"
                        [ "-o"
                        , fout
                        , f ]
                        "" --stdin
  case exit of
    ExitSuccess -> return fout
    err         -> error $ show err
  where
    fout = f -<.> "dis"

spec_edsl :: Spec
spec_edsl = do
  describe "the bitcode edsl" $ do
    it "should be able to return the CMP result" $ do
      let bcfile = "test/cmp-ret.bc"
          testModule = mod "cmp-ret"
            [ def_ "main" ([i32, ptr =<< i8ptr] --> i1) $ \[ argc, argv ] -> mdo
                block "entry" $ do
                  ret =<< (pure argc :: Edsl Symbol) `iult` (int32 2)
                pure ()
            ]
      -- To debug the module use the following.
      -- it will print the module to screen
      -- putStrLn . show . pretty $ testModule
      writeModule bcfile testModule
      decompile bcfile `shouldReturn` (bcfile -<.> "dis")
      compile bcfile `shouldReturn` (bcfile -<.> "exe")
      run (bcfile -<.> "exe") [] `shouldReturn` (1, "", "")
      run (bcfile -<.> "exe") ["x"] `shouldReturn` (0, "", "")

    it "should construct the BR statement" $ do
      let bcfile = "test/br.bc"
          testModule :: Module
          testModule = mod "branch"
            [ def_ "main" ([i32, ptr =<< i8ptr] --> i32) $ \[ argc, argv ] -> mdo
                block "entry" $ do
                  c1 <- int32 1
                  cond <- argc `iugt` c1
                  br cond one two
                one <- block "one" $ do
                  ret =<< int32 0
                two <- block "two" $ do
                  ret =<< int32 1
                pure ()
            ]
      writeModule bcfile testModule
      -- test that we can decompile the bitcode file
      decompile bcfile `shouldReturn` (bcfile -<.> "dis")
      -- test that we can compile the bitcode file
      compile bcfile `shouldReturn` (bcfile -<.> "exe")
      -- test the behaviour
      run (bcfile -<.> "exe") []    `shouldReturn` (1, "", "")
      run (bcfile -<.> "exe") ["x"] `shouldReturn` (0, "", "")

    it "should construct the SWITCH statement" $ do
      let bcfile = "test/switch.bc"
          testModule :: Module
          testModule = mod "switch"
            [ def_ "main" ([i32, ptr =<< i8ptr] --> i32) $ \[ argc, argv ] -> mdo
                block "entry" $ do
                  c1 <- int32 1
                  c2 <- int32 2
                  c3 <- int32 3
                  switch argc fallback [(c1, one)
                                       ,(c2, two)
                                       ,(c3, three)]
                fallback <- block "default" $ do
                  ret =<< int32 (-1)
                one <- block "one" $ do
                  ret =<< int32 1
                two <- block "two" $ do
                  ret =<< int32 2
                three <- block "three" $ do
                  ret =<< int32 3
                pure ()
            ]
      writeModule bcfile testModule
      decompile bcfile `shouldReturn` (bcfile -<.> "dis")
      compile bcfile `shouldReturn` (bcfile -<.> "exe")
      run (bcfile -<.> "exe") [] `shouldReturn` (1, "", "")
      run (bcfile -<.> "exe") ["x"] `shouldReturn` (2, "", "")
      run (bcfile -<.> "exe") ["x","x"] `shouldReturn` (3, "", "")
      run (bcfile -<.> "exe") ["x","x","x"] `shouldReturn` (255, "", "") -- exit codes are unsigned, hence 255

    it "should be able to call @llvm.memset" $ do
      let bcfile = "test/llvm.memset.bc"
          testModule :: Module
          testModule = mod "branch"
            [ def_ "main" ([i32, ptr =<< i8ptr] --> i32) $ \[ argc, argv ] -> mdo
                block "entry" $ do
                  slots <- bind2 alloca i8 (int8 16) 
                  memset <- fun "llvm.memset.p0i8.i32" =<< [i8ptr, i8, i32, i32, i1] --> void 
                  ccall memset =<< (slots:) <$> sequence [ int8 0, int32 16, int32 4, int 1 0]
                  ret =<< int32 0
                pure ()
            ]
      writeModule bcfile testModule
      decompile bcfile `shouldReturn` (bcfile -<.> "dis")
      compile bcfile `shouldReturn` (bcfile -<.> "exe")
      run (bcfile -<.> "exe") [] `shouldReturn` (0, "", "")

    it "sould be able to create uninitialized memory with UNDEF" $ do
      let bcfile = "test/undef.bc"
          testModule :: Module
          testModule = mod "undef"
            [ def_ "main" ([i32, ptr =<< i8ptr] --> i32) $ \[ argc, argv ] -> mdo
                block "entry" $ do
                  mem <- undef =<< (arr 10 =<< i8)
                  memG <- global "mem" mem 
                  ptr <- gep memG =<< sequence [int32 0, int32 0]
                  memset <- fun "llvm.memset.p0i8.i32" =<< [i8ptr, i8, i32, i32, i1] --> void 
                  ccall memset =<< (ptr:) <$> sequence [ int8 0, int32 10, int32 4, int 1 0 ]
                  ret =<< int32 0
                pure ()
            ]
      writeModule bcfile testModule
      decompile bcfile `shouldReturn` (bcfile -<.> "dis")
      compile bcfile `shouldReturn` (bcfile -<.> "exe")
      run (bcfile -<.> "exe") [] `shouldReturn` (0, "", "")

    it "should be able to GEP into a string" $ do
      let bcfile = "test/gep_str.bc"
          testModule :: Module
          testModule = mod "undef"
            [ def_ "main" ([i32, ptr =<< i8ptr] --> i32) $ \[ argc, argv ] -> mdo
                block "entry" $ do
                  foo <- global "foo" =<< cStr "hello world\n" 
                  strPtr  <- gep foo =<< sequence [int32 0, int32 6]
                  printf <- fun "printf" =<< (vararg $ [i8ptr] --> i32)
                  ccall printf [strPtr]
                  ret =<< int32 0
                pure ()
            ]
      -- putStrLn . show . pretty $ testModule
      writeModule bcfile testModule
      decompile bcfile `shouldReturn` (bcfile -<.> "dis")
      compile bcfile `shouldReturn` (bcfile -<.> "exe")
      run (bcfile -<.> "exe") [] `shouldReturn` (0, "world\n", "")

    it "should be able to store the arguments" $ do
      let bcfile = "test/store_args.bc"
          testModule :: Module
          testModule = mod "undef"
            [ def_ "main" ([i32, ptr =<< i8ptr] --> i32) $ \[ argc, argv ] -> mdo
                block "entry" $ do
                  argcSlot <- bind2 alloca i32 (int32 1) -- space for 1 i32
                  store argcSlot argc
            
                  argvSlot <- bind2 alloca (ptr =<< i8ptr) (int32 1)
                  store argvSlot argv
            
                  ret =<< int32 0
                pure ()
            ]
      writeModule bcfile testModule
      decompile bcfile `shouldReturn` (bcfile -<.> "dis")
      compile bcfile `shouldReturn` (bcfile -<.> "exe")
      run (bcfile -<.> "exe") [] `shouldReturn` (0, "", "")

    it "should be able to carry prefix data" $ do
      let bcfile = "test/prefix_data.bc"
          testModule :: Module
          testModule = mod "undef"
            [ withPrefixDataM_ prefix $ def_ "main" ([i32, ptr =<< i8ptr] --> i32) $ \[ argc, argv ] -> mdo
                block "entry" $ do
                  -- we will obtain the argc and argv slot to populate the local instruction and
                  -- refernece count here
                  argcSlot <- bind2 alloca i32 (int32 1) -- space for 1 i32
                  store argcSlot argc
            
                  argvSlot <- bind2 alloca (ptr =<< i8ptr) (int32 1)
                  store argvSlot argv

                  -- obtain a function pointer to @main
                  fp <- bind2 bitcast (ptr =<< ty <$> prefix) (fun "main" =<< [i32, ptr =<< i8ptr] --> i32)
                  -- get the pointer to the prefix data and load it.
                  v <- load =<< gep fp =<< sequence [int32 (-1), int32 1 ]
                  ret v
                pure ()
            ]
            where prefix = struct =<< sequence [(int32 1), (int32 10), (int32 11)]
      writeModule bcfile testModule
      decompile bcfile `shouldReturn` (bcfile -<.> "dis")
      compile bcfile `shouldReturn` (bcfile -<.> "exe")
      run (bcfile -<.> "exe") [] `shouldReturn` (10, "", "")       
            
