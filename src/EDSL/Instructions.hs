{-# LANGUAGE TupleSections #-}

module EDSL.Instructions where

import Prelude hiding (error)

import EDSL.Monad.EdslT

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

import Data.BitCode.LLVM.Pretty as P
import Text.PrettyPrint as P

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT(..), throwE, runExceptT)
import Control.Monad ((<=<), (>=>))
import Data.Functor.Identity (Identity)


import GHC.Stack
import Data.Word (Word64)

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

