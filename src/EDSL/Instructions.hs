module EDSL.Instructions where

import EDSL.Monad
import EDSL.Types

import Data.BitCode.LLVM.Classes.HasType

import Data.BitCode.LLVM.Value (Value (Function, TRef), Symbol, symbolValue)
import Data.BitCode.LLVM.Types (BasicBlockId)
import Data.BitCode.LLVM.Type  (Ty)
import Data.BitCode.LLVM.Util
import Data.BitCode.LLVM.Instruction (Inst, TailCallKind)
import Data.BitCode.LLVM.CallingConv (CallingConv)

import qualified Data.BitCode.LLVM.Instruction     as Inst
import qualified Data.BitCode.LLVM.Type            as Ty
import qualified Data.BitCode.LLVM.Cmp             as CmpOp
import qualified Data.BitCode.LLVM.Opcodes.Binary  as BinOp
import qualified Data.BitCode.LLVM.Opcodes.Cast    as CastOp
import qualified Data.BitCode.LLVM.CallingConv     as CConv

import Data.BitCode.LLVM.Pretty
import Text.PrettyPrint

-- * Instructions
allocaI :: Ty -> Symbol -> Inst
allocaI t s = Inst.Alloca (lift t) s 0 -- we want to allocate space for t, and hence want a (ptr t) to be returned.
loadI :: Symbol -> Inst
loadI s = Inst.Load (lower (ty s)) s 0
storeI :: Symbol -- ^ target (e.g where)
       -> Symbol -- ^ source (e.g. what)
       -> Inst
storeI t s | lower (ty t) == ty s = Inst.Store t s 0
           | otherwise = error $ "can not store " ++ (show (pretty s)) ++ " in " ++ (show (pretty t))
callI :: TailCallKind -> CallingConv -> Ty -> Symbol -> [Symbol] -> Inst
callI tck cc t f args | isPtr t = Inst.Call (funRetTy t) tck cc f t args
                      | otherwise = error "call expects ptr type"
retI :: Symbol -> Inst
retI = Inst.Ret . Just
retVoidI = Inst.Ret Nothing
ubrI :: BasicBlockId -> Inst
ubrI = Inst.UBr
brI :: Symbol -> BasicBlockId -> BasicBlockId -> Inst
brI = Inst.Br
switchI :: Symbol -> BasicBlockId -> [(Symbol, BasicBlockId)] -> Inst
switchI c def cases = Inst.Switch c def cases
gepI :: Symbol -> [Symbol] -> Inst
gepI s = Inst.Gep (ty s) True s

-- ** Cast
-- TODO: support FCMP as well.
truncI, zextI, sextI, fpToUiI, fpToSiI, uiToFpI, siToFpI, fpTruncI, fpExtI, ptrToIntI, intToPtrI, bitcastI, addrSpCastI :: Ty -> Symbol -> Inst
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
serror = error . show
ieqI, ineqI, iugtI, iugeI, iultI, iuleI, isgtI, isgeI, isltI, isleI :: Symbol -> Symbol -> Inst
mkCmp2 op lhs rhs | ty lhs == ty rhs = Inst.Cmp2 i1 lhs rhs op
                  | otherwise = serror $ text "*** Type Error:" <+> (text ("CMP2 (" ++ show op ++ "), types do not agree")
                                                                     $+$ text "LHS:" <+> pretty lhs
                                                                     $+$ text "RHS:" <+> pretty rhs)
ieqI  lhs rhs = mkCmp2 CmpOp.ICMP_EQ  lhs rhs
ineqI lhs rhs = mkCmp2 CmpOp.ICMP_NE  lhs rhs
iugtI lhs rhs = mkCmp2 CmpOp.ICMP_UGT lhs rhs
iugeI lhs rhs = mkCmp2 CmpOp.ICMP_UGE lhs rhs
iultI lhs rhs = mkCmp2 CmpOp.ICMP_ULT lhs rhs
iuleI lhs rhs = mkCmp2 CmpOp.ICMP_ULE lhs rhs
isgtI lhs rhs = mkCmp2 CmpOp.ICMP_SGT lhs rhs
isgeI lhs rhs = mkCmp2 CmpOp.ICMP_SGE lhs rhs
isltI lhs rhs = mkCmp2 CmpOp.ICMP_SLT lhs rhs
isleI lhs rhs = mkCmp2 CmpOp.ICMP_SLE lhs rhs
-- ** Binary Op
addI, subI, mulI, udivI, sdivI, uremI, sremI, shlI, lshrI, ashrI, andI, orI, xorI :: Symbol -> Symbol -> Inst
mkBinOp :: BinOp.BinOp -> Symbol -> Symbol -> Inst
mkBinOp op lhs rhs | ty lhs == ty rhs = Inst.BinOp (ty lhs) op lhs rhs []
                   | otherwise = serror $ text "*** Type Error:" <+> (text ("BINOP (" ++ show op ++ "), types do not agree")
                                                                      $+$ text "LHS:" <+> pretty lhs
                                                                      $+$ text "RHS:" <+> pretty rhs)

addI  lhs rhs = mkBinOp BinOp.ADD  lhs rhs
subI  lhs rhs = mkBinOp BinOp.SUB  lhs rhs
mulI  lhs rhs = mkBinOp BinOp.MUL  lhs rhs
udivI lhs rhs = mkBinOp BinOp.UDIV lhs rhs
sdivI lhs rhs = mkBinOp BinOp.SDIV lhs rhs
uremI lhs rhs = mkBinOp BinOp.UREM lhs rhs
sremI lhs rhs = mkBinOp BinOp.SREM lhs rhs
shlI  lhs rhs = mkBinOp BinOp.SHL  lhs rhs
lshrI lhs rhs = mkBinOp BinOp.LSHR lhs rhs
ashrI lhs rhs = mkBinOp BinOp.ASHR lhs rhs
andI  lhs rhs = mkBinOp BinOp.AND  lhs rhs
orI   lhs rhs = mkBinOp BinOp.OR   lhs rhs
xorI  lhs rhs = mkBinOp BinOp.XOR  lhs rhs


--------------------------------------------------------------------------------
-- Monadic interface (BodyBuilder)
-- | Intructions
alloca :: Monad m => Ty -> Symbol -> BodyBuilderT m Symbol
alloca ty size = tellInst' (allocaI ty size)
load :: Monad m => Symbol -> BodyBuilderT m Symbol
load s | isPtr (ty s) = tellInst' (loadI s)
       | otherwise    = error . show $ text "Cannlot load:" <+> pretty s <+> text "must be of pointer type!"
store :: Monad m
      => Symbol -- ^ Source
      -> Symbol -- ^ Target
      -> BodyBuilderT m ()
store source target = tellInst (storeI source target) >> pure ()
call' :: Monad m => TailCallKind -> CallingConv -> Symbol -> [Symbol] -> BodyBuilderT m (Maybe Symbol)
call' tck cc f args
  | not . isFunctionPtr . ty $ f
  = error $ show $ text "Cannot call: " <+> pretty f <+> text "must be a function pointer."
  -- TODO: length should be equal, if not vararg.
  | length args < length (funParamTys (ty f))
  = error . show $ text "Cannot call: " <+> (pretty f <+> text "with insufficent arguments."
                                              $+$ text "Params" <+> int (length (funParamTys (ty f))) <> text ":" <+> pretty (funParamTys (ty f))
                                              $+$ text "Args  " <+> int (length args) <> text ":" <+> pretty args)
  | not . Prelude.and $ zipWith (==) (map ty args) (funParamTys (ty f))
  = error . show $ text "Cannot call: " <+> (pretty f <+> text "text with arguments, not matching the signature."
                                             $+$ text "Sig :" <+> pretty (funParamTys (ty f))
                                             $+$ text "Args:" <+> pretty (map ty args))
  | (Function{}) <- symbolValue f = tellInst $ callI tck cc (ty f) f args
  | (TRef{})     <- symbolValue f = tellInst $ callI tck cc (ty f) f args
  | otherwise                     = error $ show $ text "Cannot call: " <+> pretty f

call :: Monad m => CallingConv -> Symbol -> [Symbol] -> BodyBuilderT m (Maybe Symbol)
call cc f args = call' Inst.None cc f args
tailcall :: Monad m => CallingConv -> Symbol -> [Symbol] -> BodyBuilderT m (Maybe Symbol)
tailcall cc f args = call' Inst.Tail cc f args

ccall :: Monad m => Symbol -> [Symbol] -> BodyBuilderT m (Maybe Symbol)
ccall f args = call' Inst.None CConv.C f args
ghccall :: Monad m => Symbol -> [Symbol] -> BodyBuilderT m (Maybe Symbol)
ghccall f args = call' Inst.None CConv.GHC f args

tailccall :: Monad m => Symbol -> [Symbol] -> BodyBuilderT m (Maybe Symbol)
tailccall f args = call' Inst.Tail CConv.C f args
tailghccall :: Monad m => Symbol -> [Symbol] -> BodyBuilderT m (Maybe Symbol)
tailghccall f args = call' Inst.Tail CConv.GHC f args

ret :: Monad m => Symbol -> BodyBuilderT m (Maybe Symbol)
ret = tellInst . retI
retVoid :: Monad m => BodyBuilderT m ()
retVoid = tellInst retVoidI >> pure ()

ubr :: Monad m => BasicBlockId -> BodyBuilderT m (Maybe Symbol)
ubr = tellInst . ubrI
br :: Monad m => Symbol -> BasicBlockId -> BasicBlockId -> BodyBuilderT m (Maybe Symbol)
br cond l r = tellInst (brI cond l r)
switch :: Monad m => Symbol -> BasicBlockId -> [(Symbol, BasicBlockId)] -> BodyBuilderT m (Maybe Symbol)
switch cond def cases = tellInst (switchI cond def cases)
gep :: Monad m => Symbol -> [Symbol] -> BodyBuilderT m Symbol
gep s = tellInst' . gepI s
-- ** Cast
trunc, zext, sext, fpToUi, fpToSi, uiToFp, siToFp, fpTrunc, fpExt, ptrToInt, intToPtr, bitcast, addrSpCast :: Monad m => Ty -> Symbol -> BodyBuilderT m Symbol
trunc t = tellInst' . truncI t
zext t = tellInst' . zextI t
sext t = tellInst' . sextI t
fpToUi t = tellInst' . fpToUiI t
fpToSi t = tellInst' . fpToSiI t
uiToFp t = tellInst' . uiToFpI t
siToFp t = tellInst' . siToFpI t
fpTrunc t = tellInst' . fpTruncI t
fpExt t = tellInst' . fpExtI t
ptrToInt t s | isPtr (ty s) = tellInst' (ptrToIntI t s)
             | otherwise    = error . show $ text "Cannot ptr-to-int cast: " <+> pretty s <+> text "to" <+> pretty t <+> text ", symbol not a pointer!"
intToPtr t s | isInt (ty s) = tellInst' (intToPtrI t s)
             | otherwise    = error . show $ text "Cannot int-to-ptr cast: " <+> pretty s <+> text "to" <+> pretty t <+> text ", symbol not an integer!"
bitcast t = tellInst' . bitcastI t
addrSpCast t = tellInst' . addrSpCastI t

-- ** Compare
ieq, ineq, iugt, iuge, iult, iule, isgt, isge, islt, isle :: Monad m => Symbol -> Symbol -> BodyBuilderT m Symbol
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
add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor :: Monad m => Symbol -> Symbol -> BodyBuilderT m Symbol
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
