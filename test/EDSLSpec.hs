{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecursiveDo       #-}

module EDSLSpec where

import Test.Tasty.Hspec

import Prelude hiding (mod)
import EDSL

import Data.BitCode.LLVM (Module)

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
    it "should construct the BR statement" $ do
      let bcfile = "test/br.bc"
          testModule :: Module
          testModule = mod "branch"
            [ def_ "main" ([i32, ptr i8ptr] --> i32) $ \[ argc, argv ] -> mdo
                block "entry" $ do
                  cond <- argc `iugt` int32 1
                  br cond one two
                one <- block "one" $ do
                  ret $ int32 0
                two <- block "two" $ do
                  ret $ int32 1
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
            [ def_ "main" ([i32, ptr i8ptr] --> i32) $ \[ argc, argv ] -> mdo
                block "entry" $ do
                  switch argc fallback [(int32 1, one)
                                       ,(int32 2, two)
                                       ,(int32 3, three)]
                fallback <- block "default" $ do
                  ret $ int32 (-1)
                one <- block "one" $ do
                  ret $ int32 1
                two <- block "two" $ do
                  ret $ int32 2
                three <- block "three" $ do
                  ret $ int32 3
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
            [ def_ "main" ([i32, ptr i8ptr] --> i32) $ \[ argc, argv ] -> mdo
                block "entry" $ do
                  slots <- alloca i8 (int8 16)
                  ccall (fun "llvm.memset.p0i8.i32" ([i8ptr, i8, i32, i32, i1] --> void)) [ slots, int8 0, int32 16, int32 4, int 1 0]
                  ret $ int32 0
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
            [ def_ "main" ([i32, ptr i8ptr] --> i32) $ \[ argc, argv ] -> mdo
                block "entry" $ do
                  let mem = undef (arr 10 i8)
                  ptr <- gep (global "mem" mem) [int32 0, int32 0]
                  ccall (fun "llvm.memset.p0i8.i32" ([i8ptr, i8, i32, i32, i1] --> void)) [ ptr, int8 0, int32 10, int32 4, int 1 0 ]
                  ret $ int32 0
                pure ()
            ]
      writeModule bcfile testModule
      decompile bcfile `shouldReturn` (bcfile -<.> "dis")
      compile bcfile `shouldReturn` (bcfile -<.> "exe")
      run (bcfile -<.> "exe") [] `shouldReturn` (0, "", "")
