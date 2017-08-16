module EDSL.Types where

import qualified Data.BitCode.LLVM.Type            as Ty
import qualified Data.BitCode.LLVM.Util            as Ty

-- create a pointer with 0 addressSpace
ptr :: Ty.Ty -> Ty.Ty
ptr = Ty.Ptr 0

-- Some basic types
i :: Integral a => a -> Ty.Ty
i = Ty.Int . fromIntegral
i1, i8, i32, i8ptr, i32ptr :: Ty.Ty
i1 = i 1
i8 = i 8
i16 = i 16
i32 = i 32
i64 = i 64
i8ptr = ptr i8
i32ptr = ptr i32
i64ptr = ptr i64
void = Ty.Void

f :: Integral a => a -> Ty.Ty
f 16 = f16
f 32 = f32
f 64 = f64
f 80 = f80
f 128 = f128
f _   = error "invalid float type"
half = Ty.Half
f16 = half
f32 = Ty.Float
double = Ty.Double
f64 = double
f80 = Ty.X86Fp80
f128 = Ty.Fp128
word n = Ty.Int ((fromIntegral n) * 8)
arr n t = Ty.Array (fromIntegral n) t
vec n t = Ty.Vector (fromIntegral n) t

size :: Integral a => a -> Ty.Ty -> a
size _ (Ty.Int n)       = fromIntegral n
size _ Ty.Half          = 16
size _ Ty.Float         = 32
size _ Ty.Double        = 64
size _ Ty.X86Fp80       = 80
size _ Ty.Fp128         = 128
size ptrSize (Ty.Ptr{}) = ptrSize
size _ t                = error $ "size not supported for " ++ show t

-- unpacked struct
ustruct :: [Ty.Ty] -> Ty.Ty
ustruct = Ty.StructAnon False
-- packed struct
pstruct :: [Ty.Ty] -> Ty.Ty
pstruct = Ty.StructAnon True

-- type constructors for function types
(-->) :: [Ty.Ty] -> Ty.Ty -> Ty.Ty
ts --> t = Ty.Function False t ts
vararg f@(Ty.Function{}) = f { Ty.teVarArg = True }
