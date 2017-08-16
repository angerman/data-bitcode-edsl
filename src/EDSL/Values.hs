{-# LANGUAGE RecordWildCards #-}
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
cStr_ s = Val.Constant (arr (1 + length s) i8) (v_cStr s)
str_ s = Val.Constant (arr (length s) i8) (v_str s)

-- * Constant Symbols
int_ :: Integral a => Int -> a -> Val.Symbol
int_ w = Val.Unnamed . Val.Constant (i w) . v_int . fromIntegral
int8_, int16_, int32_, int64_ :: Integral a => a -> Val.Symbol
int8_  = Val.Unnamed . Val.Constant i8 . v_int . fromIntegral
int16_ = Val.Unnamed . Val.Constant i16 . v_int . fromIntegral
int32_ = Val.Unnamed . Val.Constant i32 . v_int . fromIntegral
int64_ = Val.Unnamed . Val.Constant i64 . v_int . fromIntegral

float_ :: Int -> Rational -> Val.Symbol
float_ w = Val.Unnamed . Val.Constant (f w) . (v_float w) . fromRational
float16_, float32_, float64_, float80_, float128_ :: Rational -> Val.Symbol
float16_ = float_ 16
float32_ = float_ 32
float64_ = float_ 64
float80_ = float_ 80
float128_ = float_ 128

undef_ :: Ty.Ty -> Val.Value
undef_ = flip Val.Constant Val.Undef

undefS_ :: Ty.Ty -> Val.Symbol
undefS_ = Val.Unnamed . undef_

cStrS_ = Val.Unnamed . cStr_
strS_  = Val.Unnamed . str_

-- unpacked struct constant
-- | Construct a struct value.
struct' :: [Val.Symbol] -> Val.Value
struct' ss = Val.Constant (ustruct $ map ty ss) (Val.Struct ss)
-- | Construct a struct symbol (unnamed value)
struct :: [Val.Symbol] -> Val.Symbol
struct = Val.Unnamed . struct'

-- | low level lable creator. Do not use this in the
-- Body Builder. If you must, call tellLabel on it. Use
-- the @label@ from the Instructions.
label_ :: String -> Ty.Ty -> Val.Symbol
label_ name = Val.Named name . Val.Label

-- | create a global constant
global_ :: String -> Val.Symbol -> Val.Symbol
global_ name val = Val.Named name $ defGlobal { Val.gPointerType = ptr (ty val)
                                              , Val.gInit = Just val }
-- | create an external global constant. That is one which has no initializer.
extGlobal_ :: String -> Ty.Ty -> Val.Symbol
extGlobal_ name ty = Val.Named name $ defGlobal { Val.gPointerType = ptr ty }


-- | Mark a function as a declaration (Prototype)
mkDecl :: Val.Value -> Val.Value
mkDecl f@(Val.Function{..}) = f { Val.fExtra = fExtra { Val.feProto = True } }

-- | Mark a function as a definition (non Prototype)
mkDef :: Val.Value -> Val.Value
mkDef f@(Val.Function{..}) = f { Val.fExtra = fExtra { Val.feProto = False } }

fun_ :: String -> Ty.Ty -> Val.Symbol
fun_ name sig = Val.Named name $ mkDecl $ defFunction { Val.fType = ptr sig }

ghcfun_ :: String -> Ty.Ty -> Val.Symbol
ghcfun_ name sig = Val.Named name $ mkDef $ defFunction { Val.fType = ptr sig, Val.fCallingConv = CallingConv.GHC }

