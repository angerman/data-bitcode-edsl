module EDSL.Instructions where

import EDSL.Monad
import EDSL.Types

import Data.BitCode.LLVM (ty)
import qualified Data.BitCode.LLVM.Instruction     as Inst
import qualified Data.BitCode.LLVM.Type            as Ty
import qualified Data.BitCode.LLVM.Cmp             as CmpOp
import qualified Data.BitCode.LLVM.Opcodes.Binary  as BinOp
import qualified Data.BitCode.LLVM.Opcodes.Cast    as CastOp

-- * Instructions
allocaI t s = Inst.Alloca t s 0
loadI s = Inst.Load (ty s) s 0
storeI s t = Inst.Store s t 0
callI f args = Inst.Call (Ty.teRetTy . Ty.tePointeeTy $ ty f) f args
retI = Inst.Ret . Just
ubrI = Inst.UBr
brI = Inst.Br
switchI c def cases = Inst.Switch c def cases
gepI s = Inst.Gep (Ty.tePointeeTy sTy) True s
  where sTy = ty s

-- ** Cast
-- TODO: support FCMP as well.
truncI      t s = Inst.Cast t CastOp.TRUNC s
zextI       t s = Inst.Cast t CastOp.ZEXT s
sextI       t s = Inst.Cast t CastOp.SEXT s
fpToUiI     t s = Inst.Cast t CastOp.FPTOUI s
fpToSiI     t s = Inst.Cast t CastOp.FPTOSI s
uiToFpI     t s = Inst.Cast t CastOp.UITOFP s
siToFpI     t s = Inst.Cast t CastOp.SITOFP s
fpTruncI    t s = Inst.Cast t CastOp.FPTRUNC s
fpExtI      t s = Inst.Cast t CastOp.FPEXT s
ptrToIntI   t s = Inst.Cast t CastOp.PTRTOINT s
intToPtrI   t s = Inst.Cast t CastOp.INTTOPTR s
bitcastI    t s = Inst.Cast t CastOp.BITCAST s
addrSpCastI t s = Inst.Cast t CastOp.ADDRSPACECAST s
-- ** Compare
ieqI  lhs rhs = Inst.Cmp2 i1 lhs rhs CmpOp.ICMP_EQ
ineqI lhs rhs = Inst.Cmp2 i1 lhs rhs CmpOp.ICMP_NE
iugtI lhs rhs = Inst.Cmp2 i1 lhs rhs CmpOp.ICMP_UGT
iugeI lhs rhs = Inst.Cmp2 i1 lhs rhs CmpOp.ICMP_UGE
iultI lhs rhs = Inst.Cmp2 i1 lhs rhs CmpOp.ICMP_ULT
iuleI lhs rhs = Inst.Cmp2 i1 lhs rhs CmpOp.ICMP_ULE
isgtI lhs rhs = Inst.Cmp2 i1 lhs rhs CmpOp.ICMP_SGT
isgeI lhs rhs = Inst.Cmp2 i1 lhs rhs CmpOp.ICMP_SGE
isltI lhs rhs = Inst.Cmp2 i1 lhs rhs CmpOp.ICMP_SLT
isleI lhs rhs = Inst.Cmp2 i1 lhs rhs CmpOp.ICMP_SLE
-- ** Binary Op
addI  lhs rhs = Inst.BinOp (ty lhs) BinOp.ADD  lhs rhs []
subI  lhs rhs = Inst.BinOp (ty lhs) BinOp.SUB  lhs rhs []
mulI  lhs rhs = Inst.BinOp (ty lhs) BinOp.MUL  lhs rhs []
udivI lhs rhs = Inst.BinOp (ty lhs) BinOp.UDIV lhs rhs []
sdivI lhs rhs = Inst.BinOp (ty lhs) BinOp.SDIV lhs rhs []
uremI lhs rhs = Inst.BinOp (ty lhs) BinOp.UREM lhs rhs []
sremI lhs rhs = Inst.BinOp (ty lhs) BinOp.SREM lhs rhs []
shlI  lhs rhs = Inst.BinOp (ty lhs) BinOp.SHL  lhs rhs []
lshrI lhs rhs = Inst.BinOp (ty lhs) BinOp.LSHR lhs rhs []
ashrI lhs rhs = Inst.BinOp (ty lhs) BinOp.ASHR lhs rhs []
andI  lhs rhs = Inst.BinOp (ty lhs) BinOp.AND  lhs rhs []
orI   lhs rhs = Inst.BinOp (ty lhs) BinOp.OR   lhs rhs []
xorI  lhs rhs = Inst.BinOp (ty lhs) BinOp.XOR  lhs rhs []



--------------------------------------------------------------------------------
-- Monadic interface (BodyBuilder)
-- | Intructions
alloca ty size = tellInst' (allocaI ty size)
load = tellInst' . loadI
store source target = tellInst (storeI source target) >> pure ()
call f = tellInst' . callI f
ret = tellInst . retI
ubr = tellInst . ubrI
br cond l r = tellInst (brI cond l r)
switch cond def cases = tellInst (switchI cond def cases)
gep s = tellInst' . gepI s
-- ** Cast
-- ** Compare
ieq  lhs rhs = tellInst' (ieqI  lhs rhs)
ineq lhs rhs = tellInst' (ineqI lhs rhs)
iugt lhs rhs = tellInst' (iugtI lhs rhs)
iuge lhs rhs = tellInst' (iugeI lhs rhs)
iult lhs rhs = tellInst' (iultI lhs rhs)
iule lhs rhs = tellInst' (iuleI lhs rhs)
isgt lhs rhs = tellInst' (isgtI lhs rhs)
isge lhs rhs = tellInst' (isgeI lhs rhs)
islt lhs rhs = tellInst' (isltI lhs rhs)
isle lhs rhs = tellInst' (isleI lhs rhs)
-- ** Binary Op
add  lhs rhs = tellInst' $ addI  lhs rhs
sub  lhs rhs = tellInst' $ subI  lhs rhs
mul  lhs rhs = tellInst' $ mulI  lhs rhs
udiv lhs rhs = tellInst' $ udivI lhs rhs
sdiv lhs rhs = tellInst' $ sdivI lhs rhs
urem lhs rhs = tellInst' $ uremI lhs rhs
srem lhs rhs = tellInst' $ sremI lhs rhs
shl  lhs rhs = tellInst' $ shlI  lhs rhs
lshr lhs rhs = tellInst' $ lshrI lhs rhs
ashr lhs rhs = tellInst' $ ashrI lhs rhs
and  lhs rhs = tellInst' $ andI  lhs rhs
or   lhs rhs = tellInst' $ orI   lhs rhs
xor  lhs rhs = tellInst' $ xorI  lhs rhs
