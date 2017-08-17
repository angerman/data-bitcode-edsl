{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module EDSL.Monad.Instructions.Binary where

import EDSL.Monad.EdslT

import Data.BitCode.LLVM.Opcodes.Binary

import Data.BitCode.LLVM.Classes.HasType
import Data.BitCode.LLVM.Value (Symbol)

import qualified Data.BitCode.LLVM.Instruction as Inst

import Data.BitCode.LLVM.Pretty
import Text.PrettyPrint

import GHC.Stack (HasCallStack)

-- ** Binary Op
add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor :: (HasCallStack, Monad m) => Symbol -> Symbol -> EdslT m Symbol

mkBinOp :: (HasCallStack, Monad m) => BinOp -> Symbol -> Symbol -> EdslT m Symbol
mkBinOp op lhs rhs | ty lhs == ty rhs = tellInst' (Inst.BinOp (ty lhs) op lhs rhs [])
                   | otherwise = sthrowE $ text "*** Type Error:" <+> (text ("BINOP (" ++ show op ++ "), types do not agree")
                                                                       $+$ text "LHS:" <+> pretty lhs
                                                                       $+$ text "RHS:" <+> pretty rhs)
add  lhs rhs = mkBinOp ADD lhs rhs
sub  lhs rhs = mkBinOp SUB lhs rhs
mul  lhs rhs = mkBinOp MUL lhs rhs
udiv lhs rhs = mkBinOp UDIV lhs rhs
sdiv lhs rhs = mkBinOp SDIV lhs rhs
urem lhs rhs = mkBinOp UREM lhs rhs
srem lhs rhs = mkBinOp SREM lhs rhs
shl  lhs rhs = mkBinOp SHL lhs rhs
lshr lhs rhs = mkBinOp LSHR lhs rhs
ashr lhs rhs = mkBinOp ASHR lhs rhs
and  lhs rhs = mkBinOp AND lhs rhs
or   lhs rhs = mkBinOp OR lhs rhs
xor  lhs rhs = mkBinOp XOR lhs rhs

mkBinOp' :: (HasCallStack, Monad m) => BinOp -> EdslT m Symbol -> EdslT m Symbol -> EdslT m Symbol
mkBinOp' op lhs rhs = do lhs' <- lhs; rhs' <- rhs; mkBinOp op lhs' rhs'


addM, subM, mulM, udivM, sdivM, uremM, sremM, shlM, lshrM, ashrM, andM, orM, xorM
  :: (HasCallStack, Monad m) => EdslT m Symbol -> EdslT m Symbol -> EdslT m Symbol
addM  lhs rhs = mkBinOp' ADD lhs rhs
subM  lhs rhs = mkBinOp' SUB lhs rhs
mulM  lhs rhs = mkBinOp' MUL lhs rhs
udivM lhs rhs = mkBinOp' UDIV lhs rhs
sdivM lhs rhs = mkBinOp' SDIV lhs rhs
uremM lhs rhs = mkBinOp' UREM lhs rhs
sremM lhs rhs = mkBinOp' SREM lhs rhs
shlM  lhs rhs = mkBinOp' SHL lhs rhs
lshrM lhs rhs = mkBinOp' LSHR lhs rhs
ashrM lhs rhs = mkBinOp' ASHR lhs rhs
andM  lhs rhs = mkBinOp' AND lhs rhs
orM   lhs rhs = mkBinOp' OR lhs rhs
xorM  lhs rhs = mkBinOp' XOR lhs rhs


