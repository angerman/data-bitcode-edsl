
module EDSL.Instructions where

import Prelude hiding (error)

import EDSL.Monad
import EDSL.Types

import Data.BitCode.LLVM.Classes.HasType

import Data.BitCode.LLVM.Value (Value (Function, TRef, Constant), Named(Unnamed), Symbol, symbolValue)
import Data.BitCode.LLVM.Types (BasicBlockId)
import Data.BitCode.LLVM.Type  (Ty)
import Data.BitCode.LLVM.Function (BlockInst)
import Data.BitCode.LLVM.Util  hiding (lift)
import Data.BitCode.LLVM.Instruction (TailCallKind)
import Data.BitCode.LLVM.CallingConv (CallingConv)
import Data.BitCode.LLVM.RMWOperations (RMWOperations)
import Data.BitCode.LLVM.Codes.AtomicOrdering (AtomicOrdering)
import Data.BitCode.LLVM.Codes.SynchronizationScope (AtomicSynchScope)


import qualified Data.BitCode.LLVM.Instruction     as Inst
import qualified Data.BitCode.LLVM.Value           as Const (Const(..))
import qualified Data.BitCode.LLVM.Type            as Ty
import qualified Data.BitCode.LLVM.Cmp             as CmpOp
import qualified Data.BitCode.LLVM.Opcodes.Binary  as BinOp
import qualified Data.BitCode.LLVM.Opcodes.Cast    as CastOp
import qualified Data.BitCode.LLVM.CallingConv     as CConv
import qualified Data.BitCode.LLVM.Util            as Util
import qualified Data.BitCode.LLVM.RMWOperations   as RMWOp

import Data.BitCode.LLVM.Pretty
import Text.PrettyPrint

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT(..), throwE)
import Control.Monad ((<=<), (>=>))
import Data.Functor.Identity (Identity)

import GHC.Stack
import Data.Word (Word64)

type Error = String
type Inst = Either Error Inst.Inst

type EdslT m a = ExceptT Error (BodyBuilderT m) a
type Edsl a = ExceptT Error (BodyBuilderT Identity) a

serror = Left . show
exceptT :: Monad m => Either Error a -> EdslT m a
exceptT res = case res of
  r@(Right{}) -> ExceptT (pure r)
  (Left errMsg) -> do
    log <- lift askLog
    insts <- unlines . fmap (('\t':) . show . pretty) <$> lift (askInsts 20)
    ExceptT (pure . Left $ unlines ["LOG", log, "Last 20 instructions", insts, "ERROR", errMsg])

sthrowE :: (Monad m, Show a) => a -> ExceptT Error m b
sthrowE = throwE . show

-- * Instructions
allocaI :: HasCallStack => Ty -> Symbol -> Inst
allocaI t s = pure $ Inst.Alloca (Util.lift t) s 0 -- we want to allocate space for t, and hence want a (ptr t) to be returned.
loadI :: HasCallStack => Symbol -> Inst
loadI s = pure $ Inst.Load (lower (ty s)) s 0
storeI :: HasCallStack => Symbol -- ^ target (e.g where)
       -> Symbol -- ^ source (e.g. what)
       -> Inst
storeI t s | not (isPtr (ty t))   = Left $ "can not store " ++ (show (pretty s)) ++ " in " ++ (show (pretty t)) ++ ". Target " ++ (show (pretty t)) ++ " must be of ptr type."
           | lower (ty t) == ty s = pure $ Inst.Store t s 0
           | otherwise = Left $ "can not store " ++ (show (pretty s)) ++ " in " ++ (show (pretty t))
callI :: HasCallStack => TailCallKind -> CallingConv -> Ty -> Symbol -> [Symbol] -> Inst
callI tck cc t f args | isPtr t = pure $ Inst.Call (funRetTy t) tck cc f t args
                      | otherwise = Left "call expects ptr type"
retI :: HasCallStack => Symbol -> Inst
retI = pure . Inst.Ret . Just
retVoidI :: HasCallStack => Inst
retVoidI = pure $ Inst.Ret Nothing
ubrI :: HasCallStack => BasicBlockId -> Inst
ubrI = pure . Inst.UBr
brI :: HasCallStack => Symbol -> BasicBlockId -> BasicBlockId -> Inst
brI s b b' = pure $ Inst.Br s b b'
switchI :: HasCallStack => Symbol -> BasicBlockId -> [(Symbol, BasicBlockId)] -> Inst
switchI c def cases = pure $ Inst.Switch c def cases
gepI :: HasCallStack => Symbol -> [Symbol] -> Inst
gepI s = pure . Inst.Gep (ty s) True s
extractValueI :: HasCallStack => Symbol -> [Word64] -> Inst
extractValueI s = pure . Inst.ExtractValue s

-- ** Cast
-- TODO: support FCMP as well.
truncI, zextI, sextI, fpToUiI, fpToSiI, uiToFpI, siToFpI, fpTruncI, fpExtI, ptrToIntI, intToPtrI, bitcastI, addrSpCastI :: HasCallStack => Ty -> Symbol -> Inst
truncI      t s = pure $ Inst.Cast t CastOp.TRUNC s
zextI       t s = pure $ Inst.Cast t CastOp.ZEXT s
sextI       t s = pure $ Inst.Cast t CastOp.SEXT s
fpToUiI     t s = pure $ Inst.Cast t CastOp.FPTOUI s
fpToSiI     t s = pure $ Inst.Cast t CastOp.FPTOSI s
uiToFpI     t s = pure $ Inst.Cast t CastOp.UITOFP s
siToFpI     t s = pure $ Inst.Cast t CastOp.SITOFP s
fpTruncI    t s = pure $ Inst.Cast t CastOp.FPTRUNC s
fpExtI      t s = pure $ Inst.Cast t CastOp.FPEXT s
ptrToIntI   t s = pure $ Inst.Cast t CastOp.PTRTOINT s
intToPtrI   t s = pure $ Inst.Cast t CastOp.INTTOPTR s
bitcastI    t s = pure $ Inst.Cast t CastOp.BITCAST s
addrSpCastI t s = pure $ Inst.Cast t CastOp.ADDRSPACECAST s
-- ** Compare
ieqI, ineqI, iugtI, iugeI, iultI, iuleI, isgtI, isgeI, isltI, isleI :: HasCallStack => Symbol -> Symbol -> Inst
mkCmp2 op lhs rhs | ty lhs == ty rhs = pure $  Inst.Cmp2 i1 lhs rhs op
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
addI, subI, mulI, udivI, sdivI, uremI, sremI, shlI, lshrI, ashrI, andI, orI, xorI :: HasCallStack => Symbol -> Symbol -> Inst
mkBinOp :: HasCallStack => BinOp.BinOp -> Symbol -> Symbol -> Inst
mkBinOp op lhs rhs | ty lhs == ty rhs = pure $ Inst.BinOp (ty lhs) op lhs rhs []
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

-- ** Atomic Op

atomicLoadI :: HasCallStack => Symbol -> AtomicOrdering -> AtomicSynchScope -> Inst
atomicLoadI s o ss = pure $ Inst.AtomicLoad (lower (ty s)) s 0 o ss
atomicStoreI :: HasCallStack => {- target: -} Symbol -> {- source: -} Symbol -> AtomicOrdering -> AtomicSynchScope -> Inst
atomicStoreI t s o ss | lower (ty t) == ty s = pure $ Inst.AtomicStore t s 0 o ss
                      | otherwise = Left $ "can not atomic store " ++ (show (pretty s)) ++ " in " ++ (show (pretty t))
cmpXchgI :: HasCallStack => Symbol -> Symbol -> Symbol -> AtomicOrdering -> AtomicSynchScope -> AtomicOrdering -> Inst
cmpXchgI ptr cmp val ord scope failOrd | lower (ty ptr) == ty cmp && ty cmp == ty val = pure $ Inst.CmpXchg ptr cmp val ord scope failOrd
                                       | otherwise = serror $ text "*** TypeError:" <+> (text ("cmpXchg, types do not agree")
                                                                                         $+$ text "PTR: " <+> pretty ptr
                                                                                         $+$ text "CMP: " <+> pretty cmp
                                                                                         $+$ text "VAL: " <+> pretty val)

fenceI :: HasCallStack => AtomicOrdering -> AtomicSynchScope -> Inst
fenceI ord = pure . Inst.Fence ord

mkAtomicRMWOp :: HasCallStack => RMWOperations -> Symbol -> Symbol -> AtomicOrdering -> AtomicSynchScope -> Inst
mkAtomicRMWOp op lhs rhs ord scope | lower (ty lhs) == ty rhs = pure $ Inst.AtomicRMW lhs rhs op ord scope
                                   | otherwise = serror $ text "*** Type Error:" <+> (text ("AtomicRMW (" ++ show op ++ "), types do not agree")
                                                                                     $+$ text "PTR: " <+> pretty lhs
                                                                                     $+$ text "VAL: " <+> pretty rhs)

atomicXchgI, atomicAddI, atomicSubI, atomicAndI, atomicNandI, atomicOrI, atomicXorI, atomicMaxI, atomicMinI, atomicUmaxI, atomicUminI :: HasCallStack => Symbol -> Symbol -> AtomicOrdering -> AtomicSynchScope -> Inst
atomicXchgI lhs rhs ord scope = mkAtomicRMWOp RMWOp.XCHG lhs rhs ord scope
atomicAddI  lhs rhs ord scope = mkAtomicRMWOp RMWOp.ADD  lhs rhs ord scope
atomicSubI  lhs rhs ord scope = mkAtomicRMWOp RMWOp.SUB  lhs rhs ord scope
atomicAndI  lhs rhs ord scope = mkAtomicRMWOp RMWOp.AND  lhs rhs ord scope
atomicNandI lhs rhs ord scope = mkAtomicRMWOp RMWOp.NAND lhs rhs ord scope
atomicOrI   lhs rhs ord scope = mkAtomicRMWOp RMWOp.OR   lhs rhs ord scope
atomicXorI  lhs rhs ord scope = mkAtomicRMWOp RMWOp.XOR  lhs rhs ord scope
atomicMaxI  lhs rhs ord scope = mkAtomicRMWOp RMWOp.MAX  lhs rhs ord scope
atomicMinI  lhs rhs ord scope = mkAtomicRMWOp RMWOp.MIN  lhs rhs ord scope
atomicUmaxI lhs rhs ord scope = mkAtomicRMWOp RMWOp.UMAX lhs rhs ord scope
atomicUminI lhs rhs ord scope = mkAtomicRMWOp RMWOp.UMIN lhs rhs ord scope

-- ** Constant Cast Op
truncC, zextC, sextC, fpToUiC, fpToSiC, uiToFpC, siToFpC, fpTruncC, fpExtC, ptrToIntC, intToPtrC, bitcastC, addrSpCastC :: HasCallStack => Ty -> Symbol -> Either Error Symbol
truncC      t s = pure $ Unnamed . Constant t $ Const.Cast t CastOp.TRUNC s
zextC       t s = pure $ Unnamed . Constant t $ Const.Cast t CastOp.ZEXT s
sextC       t s = pure $ Unnamed . Constant t $ Const.Cast t CastOp.SEXT s
fpToUiC     t s = pure $ Unnamed . Constant t $ Const.Cast t CastOp.FPTOUI s
fpToSiC     t s = pure $ Unnamed . Constant t $ Const.Cast t CastOp.FPTOSI s
uiToFpC     t s = pure $ Unnamed . Constant t $ Const.Cast t CastOp.UITOFP s
siToFpC     t s = pure $ Unnamed . Constant t $ Const.Cast t CastOp.SITOFP s
fpTruncC    t s = pure $ Unnamed . Constant t $ Const.Cast t CastOp.FPTRUNC s
fpExtC      t s = pure $ Unnamed . Constant t $ Const.Cast t CastOp.FPEXT s
ptrToIntC   t s | isPtr (ty s) = pure $  Unnamed . Constant t $ Const.Cast t CastOp.PTRTOINT s
                | otherwise    = serror $ text "Cannot ptr-to-int cast: " <+> pretty s <+> text "to" <+> pretty t <+> text ", symbol not a pointer!"
intToPtrC   t s = pure $ Unnamed . Constant t $ Const.Cast t CastOp.INTTOPTR s
bitcastC    t s = pure $ Unnamed . Constant t $ Const.Cast t CastOp.BITCAST s
addrSpCastC t s = pure $ Unnamed . Constant t $ Const.Cast t CastOp.ADDRSPACECAST s

-- ** Constant Binary Op
addC, subC, mulC, udivC, sdivC, uremC, sremC, shlC, lshrC, ashrC, andC, orC, xorC :: HasCallStack => Symbol -> Symbol -> Either Error Symbol
mkConstBinOp :: HasCallStack => BinOp.BinOp -> Symbol -> Symbol -> Either Error Symbol
-- TODO: verify that both are Constants!
mkConstBinOp op lhs rhs | ty lhs == ty rhs = pure $ Unnamed (Constant (ty lhs) $ Const.BinOp op lhs rhs)
                        | otherwise = serror $ text "*** Type Error:" <+> (text ("BINOP (" ++ show op ++ "), types do not agree")
                                                                           $+$ text "LHS:" <+> pretty lhs
                                                                           $+$ text "RHS:" <+> pretty rhs)
addC  lhs rhs = mkConstBinOp BinOp.ADD  lhs rhs
subC  lhs rhs = mkConstBinOp BinOp.SUB  lhs rhs
mulC  lhs rhs = mkConstBinOp BinOp.MUL  lhs rhs
udivC lhs rhs = mkConstBinOp BinOp.UDIV lhs rhs
sdivC lhs rhs = mkConstBinOp BinOp.SDIV lhs rhs
uremC lhs rhs = mkConstBinOp BinOp.UREM lhs rhs
sremC lhs rhs = mkConstBinOp BinOp.SREM lhs rhs
shlC  lhs rhs = mkConstBinOp BinOp.SHL  lhs rhs
lshrC lhs rhs = mkConstBinOp BinOp.LSHR lhs rhs
ashrC lhs rhs = mkConstBinOp BinOp.ASHR lhs rhs
andC  lhs rhs = mkConstBinOp BinOp.AND  lhs rhs
orC   lhs rhs = mkConstBinOp BinOp.OR   lhs rhs
xorC  lhs rhs = mkConstBinOp BinOp.XOR  lhs rhs

--------------------------------------------------------------------------------
-- Monadic interface (BodyBuilder)
-- | Intructions
tellInst'' x = exceptT x >>= lift . tellInst'

alloca :: (HasCallStack, Monad m) => Ty -> Symbol -> EdslT m Symbol
alloca ty size = tellInst'' (allocaI ty size)

load :: (HasCallStack, Monad m) => Symbol -> EdslT m Symbol
load s | isPtr (ty s) = tellInst'' (loadI s)
       | otherwise    = sthrowE $ text "Cannlot load:" <+> pretty s <+> text "must be of pointer type!"
store :: (HasCallStack, Monad m)
      => Symbol -- ^ Source
      -> Symbol -- ^ Target
      -> EdslT m ()
store source target = exceptT (storeI source target) >>= lift . tellInst >> pure ()
call' :: (HasCallStack, Monad m) => TailCallKind -> CallingConv -> Symbol -> [Symbol] -> EdslT m (Maybe Symbol)
call' tck cc f args
  | not . isFunctionPtr . ty $ f
  = sthrowE $ text "Cannot call: " <+> pretty f <+> text "must be a function pointer."
  -- TODO: length should be equal, if not vararg.
  | length args < length (funParamTys (ty f))
  = sthrowE $ text "Cannot call: " <+> (pretty f <+> text "with insufficent arguments."
                                              $+$ text "Params" <+> int (length (funParamTys (ty f))) <> text ":" <+> pretty (funParamTys (ty f))
                                              $+$ text "Args  " <+> int (length args) <> text ":" <+> pretty args)
  | not . Prelude.and $ zipWith (==) (map ty args) (funParamTys (ty f))
  = sthrowE $ text "Cannot call: " <+> (pretty f <+> text "text with arguments, not matching the signature."
                                             $+$ text "Sig :" <+> pretty (funParamTys (ty f))
                                             $+$ text "Args:" <+> pretty (map ty args))
  | (Function{}) <- symbolValue f = lift . tellInst =<< exceptT (callI tck cc (ty f) f args)
  | (TRef{})     <- symbolValue f = lift . tellInst =<< exceptT (callI tck cc (ty f) f args)
  | otherwise                     = sthrowE $ text "Cannot call: " <+> pretty f

call :: (HasCallStack, Monad m) => CallingConv -> Symbol -> [Symbol] -> EdslT m (Maybe Symbol)
call cc f args = call' Inst.None cc f args
tailcall :: (HasCallStack, Monad m) => CallingConv -> Symbol -> [Symbol] -> EdslT m (Maybe Symbol)
tailcall cc f args = call' Inst.Tail cc f args

ccall :: (HasCallStack, Monad m) => Symbol -> [Symbol] -> EdslT m (Maybe Symbol)
ccall f args = call' Inst.None CConv.C f args
ghccall :: (HasCallStack, Monad m) => Symbol -> [Symbol] -> EdslT m (Maybe Symbol)
ghccall f args = call' Inst.None CConv.GHC f args

tailccall :: Monad m => Symbol -> [Symbol] -> EdslT m (Maybe Symbol)
tailccall f args = call' Inst.Tail CConv.C f args
tailghccall :: Monad m => Symbol -> [Symbol] -> EdslT m (Maybe Symbol)
tailghccall f args = call' Inst.Tail CConv.GHC f args

ret :: Monad m => Symbol -> EdslT m (Maybe Symbol)
ret = lift . tellInst <=< exceptT . retI
retVoid :: Monad m => EdslT m ()
retVoid = exceptT retVoidI >>= lift . tellInst >> pure ()

ubr :: Monad m => BasicBlockId -> EdslT m (Maybe Symbol)
ubr = lift . tellInst <=< exceptT . ubrI
br :: Monad m => Symbol -> BasicBlockId -> BasicBlockId -> EdslT m (Maybe Symbol)
br cond l r | ty cond == i1 = lift . tellInst =<< exceptT (brI cond l r)
            | otherwise     = sthrowE $ text "Cannot branch with " <+> pretty cond <+> text " condition. Must be i1"
switch :: Monad m => Symbol -> BasicBlockId -> [(Symbol, BasicBlockId)] -> EdslT m (Maybe Symbol)
switch cond def cases = lift . tellInst =<< exceptT (switchI cond def cases)
gep :: Monad m => Symbol -> [Symbol] -> EdslT m Symbol
gep s = tellInst'' . gepI s
extractValue :: (HasCallStack, Monad m) => Symbol -> [Word64] -> EdslT m Symbol
extractValue s = tellInst'' . extractValueI s

-- ** Cast
trunc, zext, sext, fpToUi, fpToSi, uiToFp, siToFp, fpTrunc, fpExt, ptrToInt, intToPtr, bitcast, addrSpCast :: Monad m => Ty -> Symbol -> EdslT m Symbol
trunc t = tellInst'' . truncI t
zext t s | ty s /= t = tellInst'' (zextI t s)
         -- LLVM will complain if this kind of cast appears in the bitcode; it considers the module broken.
         | otherwise = sthrowE $ text "Cannot zero extend cast:" <+> pretty s <+> text "to" <+> pretty t <> text "."
sext t = tellInst'' . sextI t
fpToUi t = tellInst'' . fpToUiI t
fpToSi t = tellInst'' . fpToSiI t
uiToFp t = tellInst'' . uiToFpI t
siToFp t = tellInst'' . siToFpI t
fpTrunc t = tellInst'' . fpTruncI t
fpExt t = tellInst'' . fpExtI t
ptrToInt t s | isPtr (ty s) = tellInst'' (ptrToIntI t s)
             | otherwise    = sthrowE $ text "Cannot ptr-to-int cast:" <+> pretty s <+> text "to" <+> pretty t <> text ", symbol not a pointer!"
intToPtr t s | isInt (ty s) = tellInst'' (intToPtrI t s)
             | otherwise    = sthrowE $ text "Cannot int-to-ptr cast:" <+> pretty s <+> text "to" <+> pretty t <> text ", symbol not an integer!"
bitcast t = tellInst'' . bitcastI t
addrSpCast t = tellInst'' . addrSpCastI t

-- ** Compare
ieq, ineq, iugt, iuge, iult, iule, isgt, isge, islt, isle :: Monad m => Symbol -> Symbol -> EdslT m Symbol
ieq  lhs rhs = tellInst'' (ieqI  lhs rhs)
ineq lhs rhs = tellInst'' (ineqI lhs rhs)
iugt lhs rhs = tellInst'' (iugtI lhs rhs)
iuge lhs rhs = tellInst'' (iugeI lhs rhs)
iult lhs rhs = tellInst'' (iultI lhs rhs)
iule lhs rhs = tellInst'' (iuleI lhs rhs)
isgt lhs rhs = tellInst'' (isgtI lhs rhs)
isge lhs rhs = tellInst'' (isgeI lhs rhs)
islt lhs rhs = tellInst'' (isltI lhs rhs)
isle lhs rhs = tellInst'' (isleI lhs rhs)
-- ** Binary Op
add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor :: Monad m => Symbol -> Symbol -> EdslT m Symbol
add  lhs rhs = tellInst'' $ addI  lhs rhs
sub  lhs rhs = tellInst'' $ subI  lhs rhs
mul  lhs rhs = tellInst'' $ mulI  lhs rhs
udiv lhs rhs = tellInst'' $ udivI lhs rhs
sdiv lhs rhs = tellInst'' $ sdivI lhs rhs
urem lhs rhs = tellInst'' $ uremI lhs rhs
srem lhs rhs = tellInst'' $ sremI lhs rhs
shl  lhs rhs = tellInst'' $ shlI  lhs rhs
lshr lhs rhs = tellInst'' $ lshrI lhs rhs
ashr lhs rhs = tellInst'' $ ashrI lhs rhs
and  lhs rhs = tellInst'' $ andI  lhs rhs
or   lhs rhs = tellInst'' $ orI   lhs rhs
xor  lhs rhs = tellInst'' $ xorI  lhs rhs

-- ** Atomic Op
atomicLoad :: Monad m => Symbol -> AtomicOrdering -> AtomicSynchScope -> EdslT m Symbol
atomicLoad s o ss | isPtr (ty s) = tellInst'' (atomicLoadI s o ss)
                  | otherwise    = sthrowE $ text "Cannot atomic load:" <+> pretty s <+> text "must be of pointer type!"
atomicStore :: Monad m => Symbol -> Symbol -> AtomicOrdering -> AtomicSynchScope -> EdslT m ()
atomicStore s t o ss = exceptT (atomicStoreI s t o ss) >>= lift . tellInst >> pure ()

cmpXchg :: Monad m => Symbol -> Symbol -> Symbol -> AtomicOrdering -> AtomicSynchScope -> AtomicOrdering -> EdslT m Symbol
cmpXchg ptr cmp val ord scope failOrd = tellInst'' $ cmpXchgI ptr cmp val ord scope failOrd

fence :: Monad m => AtomicOrdering -> AtomicSynchScope -> EdslT m ()
fence ord scope = exceptT (fenceI ord scope) >>= lift . tellInst >> pure ()

atomicXchg, atomicAdd, atomicSub, atomicNand, atomicOr, atomicXor, atomicMax, atomicMin, atomicUmax, atomicUmin :: Monad m => Symbol -> Symbol -> AtomicOrdering -> AtomicSynchScope -> EdslT m Symbol
atomicXchg lhs rhs ord scope = tellInst'' $ atomicXchgI lhs rhs ord scope
atomicAdd  lhs rhs ord scope = tellInst'' $ atomicAddI  lhs rhs ord scope
atomicSub  lhs rhs ord scope = tellInst'' $ atomicSubI  lhs rhs ord scope
atomicAnd  lhs rhs ord scope = tellInst'' $ atomicAndI  lhs rhs ord scope
atomicNand lhs rhs ord scope = tellInst'' $ atomicNandI lhs rhs ord scope
atomicOr   lhs rhs ord scope = tellInst'' $ atomicOrI   lhs rhs ord scope
atomicXor  lhs rhs ord scope = tellInst'' $ atomicXorI  lhs rhs ord scope
atomicMax  lhs rhs ord scope = tellInst'' $ atomicMaxI  lhs rhs ord scope
atomicMin  lhs rhs ord scope = tellInst'' $ atomicMinI  lhs rhs ord scope
atomicUmax lhs rhs ord scope = tellInst'' $ atomicUmaxI lhs rhs ord scope
atomicUmin lhs rhs ord scope = tellInst'' $ atomicUminI lhs rhs ord scope
