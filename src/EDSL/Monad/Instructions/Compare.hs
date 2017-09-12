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
-- integer
ieq, ineq, iugt, iuge, iult, iule, isgt, isge, islt, isle :: (HasCallStack, Monad m) => Symbol -> Symbol -> EdslT m Symbol
-- ordered float and unordered float (operands may be nan)
foeq, fogt, foge, folt, fole, fone, fueq, fugt, fuge, fult, fule, fune :: (HasCallStack, Monad m) => Symbol -> Symbol -> EdslT m Symbol
-- unordered float (operands may be nan)
-- ffalse, ford, funo, ftrue, are not yet provided.

mkCmp2 :: (HasCallStack, Monad m) => Predicate -> Symbol -> Symbol -> EdslT m Symbol
mkCmp2 op lhs rhs {- ty lhs == ty rhs -} = do
  t <- i1
  tellInst' (Inst.Cmp2 t lhs rhs op)
--                  | otherwise = sthrowE $ text "*** Type Error:" <+> (text ("CMP2 (" ++ show op ++ "), types do not agree")
--                                                                     $+$ text "LHS:" <+> pretty lhs
--                                                                     $+$ text "RHS:" <+> pretty rhs)

ieq  lhs rhs = traceM "IEQ" >> mkCmp2 ICMP_EQ lhs rhs
ineq lhs rhs = traceM "INE" >> mkCmp2 ICMP_NE lhs rhs
iugt lhs rhs = traceM "IUGT" >> mkCmp2 ICMP_UGT lhs rhs
iuge lhs rhs = traceM "IUGE" >> mkCmp2 ICMP_UGE lhs rhs
iult lhs rhs = traceM "IULT" >> mkCmp2 ICMP_ULT lhs rhs
iule lhs rhs = traceM "IULE" >> mkCmp2 ICMP_ULE lhs rhs
isgt lhs rhs = traceM "ISGT" >> mkCmp2 ICMP_SGT lhs rhs
isge lhs rhs = traceM "ISGE" >> mkCmp2 ICMP_SGE lhs rhs
islt lhs rhs = traceM "ISLT" >> mkCmp2 ICMP_SLT lhs rhs
isle lhs rhs = traceM "ISLE" >> mkCmp2 ICMP_SLE lhs rhs

foeq lhs rhs = traceM "FOEQ" >> mkCmp2 FCMP_OEQ lhs rhs
fogt lhs rhs = traceM "FOGT" >> mkCmp2 FCMP_OGT lhs rhs
foge lhs rhs = traceM "FOGE" >> mkCmp2 FCMP_OGE lhs rhs
folt lhs rhs = traceM "FOLT" >> mkCmp2 FCMP_OLT lhs rhs
fole lhs rhs = traceM "FOLE" >> mkCmp2 FCMP_OLE lhs rhs
fone lhs rhs = traceM "FONE" >> mkCmp2 FCMP_ONE lhs rhs

fueq lhs rhs = traceM "FUEQ" >> mkCmp2 FCMP_UEQ lhs rhs
fugt lhs rhs = traceM "FUGT" >> mkCmp2 FCMP_UGT lhs rhs
fuge lhs rhs = traceM "FUGE" >> mkCmp2 FCMP_UGE lhs rhs
fult lhs rhs = traceM "FULT" >> mkCmp2 FCMP_ULT lhs rhs
fule lhs rhs = traceM "FULE" >> mkCmp2 FCMP_ULE lhs rhs
fune lhs rhs = traceM "FUNE" >> mkCmp2 FCMP_UNE lhs rhs

mkCmp2' :: (HasCallStack, Monad m) => Predicate -> EdslT m Symbol -> EdslT m Symbol -> EdslT m Symbol
mkCmp2' op lhs rhs = do lhs' <- lhs; rhs' <- rhs; mkCmp2 op lhs' rhs'

ieqM  lhs rhs = traceM "IEQ'" >> mkCmp2' ICMP_EQ lhs rhs
ineqM lhs rhs = traceM "INE'" >> mkCmp2' ICMP_NE lhs rhs
iugtM lhs rhs = traceM "IUGT'" >> mkCmp2' ICMP_UGT lhs rhs
iugeM lhs rhs = traceM "IUGE'" >> mkCmp2' ICMP_UGE lhs rhs
iultM lhs rhs = traceM "IULT'" >> mkCmp2' ICMP_ULT lhs rhs
iuleM lhs rhs = traceM "IULE'" >> mkCmp2' ICMP_ULE lhs rhs
isgtM lhs rhs = traceM "ISGT'" >> mkCmp2' ICMP_SGT lhs rhs
isgeM lhs rhs = traceM "ISGE'" >> mkCmp2' ICMP_SGE lhs rhs
isltM lhs rhs = traceM "ISLT'" >> mkCmp2' ICMP_SLT lhs rhs
isleM lhs rhs = traceM "ISLE'" >> mkCmp2' ICMP_SLE lhs rhs

foeqM lhs rhs = traceM "FOEQ'" >> mkCmp2' FCMP_OEQ lhs rhs
fogtM lhs rhs = traceM "FOGT'" >> mkCmp2' FCMP_OGT lhs rhs
fogeM lhs rhs = traceM "FOGE'" >> mkCmp2' FCMP_OGE lhs rhs
foltM lhs rhs = traceM "FOLT'" >> mkCmp2' FCMP_OLT lhs rhs
foleM lhs rhs = traceM "FOLE'" >> mkCmp2' FCMP_OLE lhs rhs
foneM lhs rhs = traceM "FONE'" >> mkCmp2' FCMP_ONE lhs rhs

fueqM lhs rhs = traceM "FUEQ'" >> mkCmp2' FCMP_UEQ lhs rhs
fugtM lhs rhs = traceM "FUGT'" >> mkCmp2' FCMP_UGT lhs rhs
fugeM lhs rhs = traceM "FUGE'" >> mkCmp2' FCMP_UGE lhs rhs
fultM lhs rhs = traceM "FULT'" >> mkCmp2' FCMP_ULT lhs rhs
fuleM lhs rhs = traceM "FULE'" >> mkCmp2' FCMP_ULE lhs rhs
funeM lhs rhs = traceM "FUNE'" >> mkCmp2' FCMP_UNE lhs rhs
