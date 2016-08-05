module EDSL.Values where

import EDSL.Types

import qualified Data.BitCode.LLVM.Value           as Val
import qualified Data.BitCode.LLVM.Linkage         as Linkage
import qualified Data.BitCode.LLVM.Visibility      as Visibility
import qualified Data.BitCode.LLVM.ThreadLocalMode as ThreadLocalMode
import qualified Data.BitCode.LLVM.StorageClass    as StorageClass
import qualified Data.BitCode.LLVM.CallingConv     as CallingConv

-- values
v_null = Val.Null
v_int n = Val.Int n
v_wInt [n] = Val.WideInt n
v_str s = Val.String s
v_cStr s = Val.CString s

-- Some def'ault constructors
defLinkage = Linkage.External
defVisibility = Visibility.Default
defTLM = ThreadLocalMode.NotThreadLocal
defStorageClass = StorageClass.Default
defCC = CallingConv.C
defSymbol :: Val.Symbol
defSymbol = undefined

-- Globals (globals, functions, aliases)
defGlobal, defFunction, defAlias :: Val.Value
defGlobal   = Val.Global void True 0 Nothing defLinkage 0 0 defVisibility defTLM False False defStorageClass 0
defFunction = Val.Function void defCC True defLinkage 0 0 0 defVisibility 0 False 0 defStorageClass 0 0 0
defAlias    = Val.Alias void 0 defSymbol defLinkage defVisibility defTLM False defStorageClass

-- * Constant Values
cStr s = Val.Constant (arr (1 + length s) i8) (v_cStr s)
str s = Val.Constant (arr (length s) i8) (v_str s)

-- * Constant Symbols
int8  = Val.Unnamed . Val.Constant i8 . v_int
int32 = Val.Unnamed . Val.Constant i32 . v_int
int64 = Val.Unnamed . Val.Constant i64 . v_int
