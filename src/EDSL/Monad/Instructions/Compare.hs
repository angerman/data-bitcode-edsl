{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module EDSL.Monad.Instructions.Compare where

import EDSL.Monad.EdslT
import EDSL.Monad.Types

import Data.BitCode.LLVM.Cmp

import Data.BitCode.LLVM.Classes.HasType
import Data.BitCode.LLVM.Value (Symbol)
import Data.BitCode.LLVM.Util
import qualified Data.BitCode.LLVM.Instruction as Inst

import Data.BitCode.LLVM.Pretty
import Text.PrettyPrint

import GHC.Stack (HasCallStack)

-- ** Compare
class CompareOp a where
  ieq, ineq, iugt, iuge, iult, iule, isgt, isge, islt, isle :: (HasCallStack) => a -> a -> Edsl Symbol

mkCmp2 :: (HasCallStack, Monad m) => Predicate -> Symbol -> Symbol -> EdslT m Symbol
mkCmp2 op lhs rhs | ty lhs == ty rhs = tellInst' =<< Inst.Cmp2 <$> i1 <*> pure lhs <*> pure rhs <*> pure op
                  | otherwise = sthrowE $ text "*** Type Error:" <+> (text ("CMP2 (" ++ show op ++ "), types do not agree")
                                                                     $+$ text "LHS:" <+> pretty lhs
                                                                     $+$ text "RHS:" <+> pretty rhs)

instance CompareOp Symbol where
  ieq  lhs rhs = mkCmp2 ICMP_EQ lhs rhs
  ineq lhs rhs = mkCmp2 ICMP_NE lhs rhs
  iugt lhs rhs = mkCmp2 ICMP_UGT lhs rhs
  iuge lhs rhs = mkCmp2 ICMP_UGE lhs rhs
  iult lhs rhs = mkCmp2 ICMP_ULT lhs rhs
  iule lhs rhs = mkCmp2 ICMP_ULE lhs rhs
  isgt lhs rhs = mkCmp2 ICMP_SGT lhs rhs
  isge lhs rhs = mkCmp2 ICMP_SGE lhs rhs
  islt lhs rhs = mkCmp2 ICMP_SLT lhs rhs
  isle lhs rhs = mkCmp2 ICMP_SLE lhs rhs

mkCmp2' :: (HasCallStack) => Predicate -> Edsl Symbol -> Edsl Symbol -> Edsl Symbol
mkCmp2' op lhs rhs = do lhs' <- lhs; rhs' <- rhs; mkCmp2 op lhs' rhs'

instance CompareOp (Edsl Symbol) where
  ieq  lhs rhs = mkCmp2' ICMP_EQ lhs rhs
  ineq lhs rhs = mkCmp2' ICMP_NE lhs rhs
  iugt lhs rhs = mkCmp2' ICMP_UGT lhs rhs
  iuge lhs rhs = mkCmp2' ICMP_UGE lhs rhs
  iult lhs rhs = mkCmp2' ICMP_ULT lhs rhs
  iule lhs rhs = mkCmp2' ICMP_ULE lhs rhs
  isgt lhs rhs = mkCmp2' ICMP_SGT lhs rhs
  isge lhs rhs = mkCmp2' ICMP_SGE lhs rhs
  islt lhs rhs = mkCmp2' ICMP_SLT lhs rhs
  isle lhs rhs = mkCmp2' ICMP_SLE lhs rhs

