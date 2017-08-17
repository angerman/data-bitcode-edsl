module EDSL.Monad.Instructions.Atomic where

import EDSL.Monad.EdslT
import EDSL.Monad.Types
import Data.BitCode.LLVM.RMWOperations
import Data.BitCode.LLVM.Codes.AtomicOrdering (AtomicOrdering)
import Data.BitCode.LLVM.Codes.SynchronizationScope (AtomicSynchScope)

import Data.BitCode.LLVM.Classes.HasType
import Data.BitCode.LLVM.Value (Symbol)
import Data.BitCode.LLVM.Util
import qualified Data.BitCode.LLVM.Instruction     as Inst

import Data.BitCode.LLVM.Pretty
import Text.PrettyPrint

import GHC.Stack (HasCallStack)

-- ** Atomic Op
atomicLoad :: (HasCallStack, Monad m) => Symbol -> AtomicOrdering -> AtomicSynchScope -> EdslT m Symbol
atomicLoad s o ss | isPtr (ty s) = tellInst' =<< Inst.AtomicLoad <$> deptr (ty s) <*> pure s <*> pure 0 <*> pure o <*> pure ss
                  | otherwise    = sthrowE $ text "Cannot atomic load:" <+> pretty s <+> text "must be of pointer type!"
atomicStore :: (HasCallStack, Monad m) => Symbol -> Symbol -> AtomicOrdering -> AtomicSynchScope -> EdslT m ()
atomicStore s t o ss | lower (ty t) == ty s = tellInst (Inst.AtomicStore t s 0 o ss) >> pure ()
                     | otherwise = sthrowE $ text "can not atomic store:" <+> pretty s <+> text "in" <+> pretty t

cmpXchg :: (HasCallStack, Monad m) => Symbol -> Symbol -> Symbol -> AtomicOrdering -> AtomicSynchScope -> AtomicOrdering -> EdslT m Symbol
cmpXchg ptr cmp val ord scope failOrd | lower (ty ptr) == ty cmp && ty cmp == ty val = tellInst' (Inst.CmpXchg ptr cmp val ord scope failOrd)
                                      | otherwise = sthrowE $ text "*** TypeError:" <+> (text ("cmpXchg, types do not agree")
                                                                                         $+$ text "PTR: " <+> pretty ptr
                                                                                         $+$ text "CMP: " <+> pretty cmp
                                                                                         $+$ text "VAL: " <+> pretty val)

fence :: (HasCallStack, Monad m) => AtomicOrdering -> AtomicSynchScope -> EdslT m ()
fence ord scope = tellInst (Inst.Fence ord scope) >> pure ()

atomicXchg, atomicAdd, atomicSub, atomicNand, atomicOr, atomicXor, atomicMax, atomicMin, atomicUmax, atomicUmin
  :: (HasCallStack, Monad m) => Symbol -> Symbol -> AtomicOrdering -> AtomicSynchScope -> EdslT m Symbol
mkAtomicOp :: (HasCallStack, Monad m) => RMWOperations -> Symbol -> Symbol -> AtomicOrdering -> AtomicSynchScope -> EdslT m Symbol
mkAtomicOp op lhs rhs ord scope | lower (ty lhs) == ty rhs = tellInst' (Inst.AtomicRMW lhs rhs op ord scope)

atomicXchg lhs rhs ord scope = mkAtomicOp XCHG lhs rhs ord scope
atomicAdd  lhs rhs ord scope = mkAtomicOp ADD  lhs rhs ord scope
atomicSub  lhs rhs ord scope = mkAtomicOp SUB  lhs rhs ord scope
atomicAnd  lhs rhs ord scope = mkAtomicOp AND  lhs rhs ord scope
atomicNand lhs rhs ord scope = mkAtomicOp NAND lhs rhs ord scope
atomicOr   lhs rhs ord scope = mkAtomicOp OR   lhs rhs ord scope
atomicXor  lhs rhs ord scope = mkAtomicOp XOR  lhs rhs ord scope
atomicMax  lhs rhs ord scope = mkAtomicOp MAX  lhs rhs ord scope
atomicMin  lhs rhs ord scope = mkAtomicOp MIN  lhs rhs ord scope
atomicUmax lhs rhs ord scope = mkAtomicOp UMAX lhs rhs ord scope
atomicUmin lhs rhs ord scope = mkAtomicOp UMIN lhs rhs ord scope
