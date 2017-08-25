{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE RecordWildCards #-}
module EDSL.Monad.Values where

-- import EDSL.Monad.Internal
import EDSL.Monad.EdslT
import EDSL.Monad.Types
import EDSL.Monad.Default

import Data.BitCode.LLVM.Type (Ty, isPtr)
import Data.BitCode.LLVM.Value
import Data.BitCode.LLVM.CallingConv (CallingConv)
import Data.BitCode.LLVM.Classes.HasType
import qualified Data.BitCode.LLVM.CallingConv     as CallingConv
import qualified Data.BitCode.LLVM.Linkage         as Linkage
import qualified Data.BitCode.LLVM.Visibility      as Visibility
import qualified Data.BitCode.LLVM.ThreadLocalMode as ThreadLocalMode
import qualified Data.BitCode.LLVM.StorageClass    as StorageClass

import Data.Word (Word16, Word32, Word64)
import Data.Ratio (numerator)

import GHC.Stack (HasCallStack)


bind2 f x y = do x' <- x; y' <- y; f x' y'

withInit :: Symbol -> Value -> Value
withInit s v@(Global{}) = v { gInit = Just (trace "[withInit] accessing init" s) }

withCC :: CallingConv -> Value -> Value
withCC cc v@(Function{}) = v { fCallingConv = cc }

-- | Mark a function as a declaration (Prototype)
mkDecl :: Value -> Value
mkDecl f@(Function{..}) = f { fExtra = fExtra { feProto = True } }

-- | Mark a function as a definition (non Prototype)
mkDef :: Value -> Value
mkDef f@(Function{..}) = f { fExtra = fExtra { feProto = False } }

labelWithValue :: (HasCallStack, Monad m) => String -> Value -> EdslT m Symbol
labelWithValue name val = tellLabel name (ty val) (Just val)

-- | create a typed label to be resolved later.
label :: (HasCallStack, Monad m) => String -> Ty -> EdslT m Symbol
label name ty = tellLabel name ty Nothing 

-- | Globals (tracked in the monad)
global :: (HasCallStack, Monad m) => String -> Symbol -> EdslT m Symbol
global name val = do
  t <- ptr (ty val)
  tellGlobal' . mkNamed t name . trace ("[global] accessing global " ++ name) . withInit val . defGlobal $ t

extGlobal :: (HasCallStack, Monad m) => String -> Ty -> EdslT m Symbol
extGlobal name ty = tellGlobal . mkNamed ty name =<< defGlobal <$> (ptr ty)

-- | INTERNAL: this will *not* record the type, nor the created global.
extGlobal_ :: HasCallStack => String -> Ty -> Symbol
extGlobal_ name ty | isPtr ty = mkNamed ty name (defGlobal ty)
                   | otherwise = error $ "ty " ++ show ty ++ " must be ptr for global " ++ show name 

extValue_ :: HasCallStack => Ty -> String -> Value -> Symbol
extValue_ t name = mkNamed t name

fun :: (HasCallStack, Monad m) => String -> Ty -> EdslT m Symbol
fun name sig = labelWithValue name =<< mkDecl . defFunction <$> ptr sig

deffun :: (HasCallStack, Monad m) => (Value -> Value) -> String -> Ty -> EdslT m Symbol
deffun mod name sig = do
  t <- ptr sig
  tellFunc . mkNamed t name . mod . mkDef . defFunction $ t

ghcfun :: (HasCallStack, Monad m) => String -> Ty -> EdslT m Symbol
ghcfun name sig = labelWithValue name =<< mkDecl . withCC CallingConv.GHC . defFunction <$> ptr sig 

defghcfun :: (HasCallStack, Monad m) => (Value -> Value) -> String -> Ty -> EdslT m Symbol
defghcfun mod name sig = do
  t <- ptr sig
  tellFunc . mkNamed t name . mod . mkDef . withCC CallingConv.GHC . defFunction $ t

uconst :: Ty -> Const -> Symbol
uconst t = mkUnnamed t . trace "[uconst]" . Constant t

-- | Constants (tracked in the monad)
-- | @cStr@ creates a null terminated c string.
cStr :: (HasCallStack, Monad m) => String -> EdslT m Symbol
cStr s = tellConst =<< uconst <$> (arr (1 + length s) =<< i8) <*> pure (CString s)

-- | @str@ creates a string, non-@\0@ terminated.
str :: (HasCallStack, Monad m) => String -> EdslT m Symbol
str s = tellConst =<< uconst <$> (arr (length s) =<< i8) <*> pure (String s)

-- unpacked struct constant
-- | Construct a struct symbol (unnamed value)
struct :: (HasCallStack, Monad m) => [Symbol] -> EdslT m Symbol
struct ss = tellConst =<< uconst <$> (ustruct $ map ty ss) <*> pure (trace "accessing struct" (Struct (trace "accessing struct symbols" ss)))

int :: (HasCallStack, Monad m, Integral a, Integral b) => a -> b -> EdslT m Symbol
int w n = tellConst =<< trace "[uconst const int]" . uconst . trace "[const int]" <$> (i w) <*> pure (Int (fromIntegral n))

int8, int16, int32, int64 :: (HasCallStack, Monad m, Integral a) => a -> EdslT m Symbol
int8  = int 8
int16 = int 16
int32 = int 32
int64 = int 64

float :: (HasCallStack, Monad m) => Int -> Rational -> EdslT m Symbol
float w r = tellConst =<< uconst <$> (f w) <*> pure (mkF w r)
  where mkF w r
          | w == 16  = Float (FpHalf      (toWord16 r))
          | w == 32  = Float (FpSingle    (toWord32 r))
          | w == 64  = Float (FpDouble    (toWord64 r))
          | w == 80  = Float (FpDoubleExt (toDoubleExt r))
          | w == 128 = Float (FpQuad      (toQuad r))
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

float16, float32, float64, float80, float128 :: (HasCallStack, Monad m) => Rational -> EdslT m Symbol
float16 = float 16
float32 = float 32
float64 = float 64
float80 = float 80
float128 = float128

undef :: (HasCallStack, Monad m) => Ty -> EdslT m Symbol
undef t = tellConst . mkUnnamed t $ Constant t Undef 

