{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}
module EDSL.Monad.Types where

import EDSL.Monad.EdslT
-- import EDSL.Monad.Internal
--import EDSL.Types
import Data.BitCode.LLVM.Type
import Data.BitCode.LLVM.Util

import Control.Monad ((<=<))

import GHC.Stack (HasCallStack)

ptr :: (HasCallStack, Monad m) => Ty -> EdslT m Ty
ptr = tellType . Ptr 0

deptr :: (HasCallStack, Monad m) => Ty -> EdslT m Ty
deptr (Ptr _ t) = pure t
deptr t = error $ "cannot lower type: " ++ show t

i :: (HasCallStack, Monad m, Integral a) => a -> EdslT m Ty
i = tellType . Int . fromIntegral

i1, i8, i16, i32, i64, i8ptr, i16ptr, i32ptr, i64ptr, void :: (HasCallStack, Monad m) => EdslT m Ty
i1 = i 1
i8 = i 8
i16 = i 16
i32 = i 32
i64 = i 64
i8ptr = ptr =<< i8
i16ptr = ptr =<< i16
i32ptr = ptr =<< i32
i64ptr = ptr =<< i64
void = tellType Void

f :: (HasCallStack, Monad m, Integral a) => a -> EdslT m Ty
f 16 = f16
f 32 = f32
f 64 = f64
f 80 = f80
f 128 = f128
f _ = error "invalid float type"

f16, f32, f64, f80, f128 :: (HasCallStack, Monad m) => EdslT m Ty
f16 = tellType Half
f32 = tellType Float
f64 = tellType Double
f80 = tellType X86Fp80
f128 = tellType Fp128

word n = i (n * 8) 
arr n = tellType . Array (fromIntegral n) 
vec n = tellType . Vector (fromIntegral n)

ustruct, pstruct :: Monad m => [Ty] -> EdslT m Ty
ustruct = tellType . StructAnon False
pstruct = tellType . StructAnon True

(-->) :: Monad m => [EdslT m Ty] -> EdslT m Ty -> EdslT m Ty
ts --> t = tellType =<< Function False <$> t <*> sequence ts

(++>) :: Monad m => [Ty] -> Ty -> EdslT m Ty
ts ++> t = tellType (Function False t ts)

vararg :: Monad m => EdslT m Ty -> EdslT m Ty
vararg f = tellType =<< vararg' <$> f
  where vararg' :: Ty -> Ty
        vararg' f@(Function{}) = f { teVarArg = True }

size :: Integral a => a -> Ty -> a
size _ (Int n)       = fromIntegral n
size _ Half          = 16
size _ Float         = 32
size _ Double        = 64
size _ X86Fp80       = 80
size _ Fp128         = 128
size ptrSize (Ptr{}) = ptrSize
size _ t                = error $ "size not supported for " ++ show t
