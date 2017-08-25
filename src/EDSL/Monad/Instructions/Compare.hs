{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module EDSL.Monad.Instructions.Compare where

import EDSL.Monad.EdslT
import EDSL.Monad.Types

import Data.BitCode.LLVM.Cmp

import Data.BitCode.LLVM.Classes.HasType
import Data.BitCode.LLVM.Value (Symbol, traceM)
import Data.BitCode.LLVM.Util
import qualified Data.BitCode.LLVM.Instruction as Inst

import Data.BitCode.LLVM.Pretty
import Text.PrettyPrint

import GHC.Stack (HasCallStack)


-- ** Compare
ieq, ineq, iugt, iuge, iult, iule, isgt, isge, islt, isle :: (HasCallStack, Monad m) => Symbol -> Symbol -> EdslT m Symbol

mkCmp2 :: (HasCallStack, Monad m) => Predicate -> Symbol -> Symbol -> EdslT m Symbol
mkCmp2 op lhs rhs {- ty lhs == ty rhs -} = do
  t <- i1
  tellInst' (Inst.Cmp2 t lhs rhs op)
--                  | otherwise = sthrowE $ text "*** Type Error:" <+> (text ("CMP2 (" ++ show op ++ "), types do not agree")
--                                                                     $+$ text "LHS:" <+> pretty lhs
--                                                                     $+$ text "RHS:" <+> pretty rhs)

ieq  lhs rhs = traceM "EQ" >> mkCmp2 ICMP_EQ lhs rhs
ineq lhs rhs = traceM "NE" >> mkCmp2 ICMP_NE lhs rhs
iugt lhs rhs = traceM "UGT" >> mkCmp2 ICMP_UGT lhs rhs
iuge lhs rhs = traceM "UGE" >> mkCmp2 ICMP_UGE lhs rhs
iult lhs rhs = traceM "ULT" >> mkCmp2 ICMP_ULT lhs rhs
iule lhs rhs = traceM "ULE" >> mkCmp2 ICMP_ULE lhs rhs
isgt lhs rhs = traceM "SGT" >> mkCmp2 ICMP_SGT lhs rhs
isge lhs rhs = traceM "SGE" >> mkCmp2 ICMP_SGE lhs rhs
islt lhs rhs = traceM "SLT" >> mkCmp2 ICMP_SLT lhs rhs
isle lhs rhs = traceM "SLE" >> mkCmp2 ICMP_SLE lhs rhs

mkCmp2' :: (HasCallStack, Monad m) => Predicate -> EdslT m Symbol -> EdslT m Symbol -> EdslT m Symbol
mkCmp2' op lhs rhs = do lhs' <- lhs; rhs' <- rhs; mkCmp2 op lhs' rhs'

ieqM  lhs rhs = traceM "EQ'" >> mkCmp2' ICMP_EQ lhs rhs
ineqM lhs rhs = traceM "NE'" >> mkCmp2' ICMP_NE lhs rhs
iugtM lhs rhs = traceM "UGT'" >> mkCmp2' ICMP_UGT lhs rhs
iugeM lhs rhs = traceM "UGE'" >> mkCmp2' ICMP_UGE lhs rhs
iultM lhs rhs = traceM "ULT'" >> mkCmp2' ICMP_ULT lhs rhs
iuleM lhs rhs = traceM "ULE'" >> mkCmp2' ICMP_ULE lhs rhs
isgtM lhs rhs = traceM "SGT'" >> mkCmp2' ICMP_SGT lhs rhs
isgeM lhs rhs = traceM "SGE'" >> mkCmp2' ICMP_SGE lhs rhs
isltM lhs rhs = traceM "SLT'" >> mkCmp2' ICMP_SLT lhs rhs
isleM lhs rhs = traceM "SLE'" >> mkCmp2' ICMP_SLE lhs rhs

