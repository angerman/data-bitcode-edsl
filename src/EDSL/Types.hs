module EDSL.Types where

import qualified Data.BitCode.LLVM.Type            as Ty

-- create a pointer with 0 addressSpace
ptr :: Ty.Ty -> Ty.Ty
ptr = Ty.Ptr 0

-- Some basic types
i1, i8, i32, i8ptr, i32ptr :: Ty.Ty
i1 = Ty.Int 1
i8 = Ty.Int 8
i32 = Ty.Int 32
i64 = Ty.Int 64
i8ptr = ptr i8
i32ptr = ptr i32
i64ptr = ptr i64
void = Ty.Void
arr n t = Ty.Array (fromIntegral n) t

-- type constructors for function types
(-->) :: [Ty.Ty] -> Ty.Ty -> Ty.Ty
ts --> t = Ty.Function False t ts
vararg f@(Ty.Function{}) = f { Ty.teVarArg = True }
