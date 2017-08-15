module EDSL.Values where

import EDSL.Types

import Data.BitCode.LLVM.Classes.HasType (ty)
import qualified Data.BitCode.LLVM.Type            as Ty
import qualified Data.BitCode.LLVM.Value           as Val
import qualified Data.BitCode.LLVM.Linkage         as Linkage
import qualified Data.BitCode.LLVM.Visibility      as Visibility
import qualified Data.BitCode.LLVM.ThreadLocalMode as ThreadLocalMode
import qualified Data.BitCode.LLVM.StorageClass    as StorageClass
import qualified Data.BitCode.LLVM.CallingConv     as CallingConv

import Data.Word (Word16, Word32, Word64)
import Data.Bits (shift, (.|.))
import Data.Ratio (numerator)

-- values
v_null = Val.Null
v_int n = Val.Int n
v_wInt [n] = Val.WideInt n
v_float w r
  | w == 16  = Val.Float (Val.FpHalf      (toWord16 r))
  | w == 32  = Val.Float (Val.FpSingle    (toWord32 r))
  | w == 64  = Val.Float (Val.FpDouble    (toWord64 r))
  | w == 80  = Val.Float (Val.FpDoubleExt (toDoubleExt r))
  | w == 128 = Val.Float (Val.FpQuad      (toQuad r))
  where toWord16 :: Rational -> Word16
        toWord16 r | numerator r == 0 = 0
                   | otherwise        = undefined
        toWord32 :: Rational -> Word32
        toWord32 = undefined
        toWord64 :: Rational -> Word64
        toWord64 = undefined
        toDoubleExt :: Rational -> (Word64, Word64)
        toDoubleExt = undefined
        toQuad :: Rational -> (Word64, Word64)
        toQuad = undefined
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
defFunction = Val.Function void defCC defLinkage 0 0 0 defVisibility 0 False defStorageClass 0 0 (Val.FE True Nothing Nothing)
defAlias    = Val.Alias void 0 defSymbol defLinkage defVisibility defTLM False defStorageClass

-- * Constant Values
cStr s = Val.Constant (arr (1 + length s) i8) (v_cStr s)
str s = Val.Constant (arr (length s) i8) (v_str s)

-- * Constant Symbols
int w = Val.Unnamed . Val.Constant (i w) . v_int . fromIntegral
int8  = Val.Unnamed . Val.Constant i8 . v_int
int32 = Val.Unnamed . Val.Constant i32 . v_int
int64 = Val.Unnamed . Val.Constant i64 . v_int

float w = Val.Unnamed . Val.Constant (f w) . (v_float w) . fromRational
float16 = float 16
float32 = float 32
float64 = float 64
float80 = float 80
float128 = float 128

undef :: Ty.Ty -> Val.Value
undef = flip Val.Constant Val.Undef

undefS :: Ty.Ty -> Val.Symbol
undefS = Val.Unnamed . undef

cStrS = Val.Unnamed . cStr
strS  = Val.Unnamed . str

-- unpacked struct constant
-- | Construct a struct value.
struct' :: [Val.Symbol] -> Val.Value
struct' ss = Val.Constant (ustruct $ map ty ss) (Val.Struct ss)
-- | Construct a struct symbol (unnamed value)
struct :: [Val.Symbol] -> Val.Symbol
struct = Val.Unnamed . struct'
