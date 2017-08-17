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
class BinaryOp a where
  add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor :: (HasCallStack) => a -> a -> Edsl Symbol

mkBinOp :: (HasCallStack, Monad m) => BinOp -> Symbol -> Symbol -> EdslT m Symbol
mkBinOp op lhs rhs | ty lhs == ty rhs = tellInst' (Inst.BinOp (ty lhs) op lhs rhs [])
                   | otherwise = sthrowE $ text "*** Type Error:" <+> (text ("BINOP (" ++ show op ++ "), types do not agree")
                                                                       $+$ text "LHS:" <+> pretty lhs
                                                                       $+$ text "RHS:" <+> pretty rhs)
instance BinaryOp Symbol where
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

mkBinOp' :: (HasCallStack) => BinOp -> Edsl Symbol -> Edsl Symbol -> Edsl Symbol
mkBinOp' op lhs rhs = do lhs' <- lhs; rhs' <- rhs; mkBinOp op lhs' rhs'

instance BinaryOp (Edsl Symbol) where
  add  lhs rhs = mkBinOp' ADD lhs rhs
  sub  lhs rhs = mkBinOp' SUB lhs rhs
  mul  lhs rhs = mkBinOp' MUL lhs rhs
  udiv lhs rhs = mkBinOp' UDIV lhs rhs
  sdiv lhs rhs = mkBinOp' SDIV lhs rhs
  urem lhs rhs = mkBinOp' UREM lhs rhs
  srem lhs rhs = mkBinOp' SREM lhs rhs
  shl  lhs rhs = mkBinOp' SHL lhs rhs
  lshr lhs rhs = mkBinOp' LSHR lhs rhs
  ashr lhs rhs = mkBinOp' ASHR lhs rhs
  and  lhs rhs = mkBinOp' AND lhs rhs
  or   lhs rhs = mkBinOp' OR lhs rhs
  xor  lhs rhs = mkBinOp' XOR lhs rhs
