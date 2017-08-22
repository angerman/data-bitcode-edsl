{-# LANGUAGE TupleSections #-}

module EDSL.Monad.Instructions.Constant where

--import Prelude hiding (error)

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
import Data.BitCode.LLVM.Opcodes.Cast (CastOp)

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
truncC, zextC, sextC, fpToUiC, fpToSiC, uiToFpC, siToFpC, fpTruncC, fpExtC, ptrToIntC, intToPtrC, bitcastC, addrSpCastC
  :: (Monad m, HasCallStack) => Ty -> Symbol -> EdslT m Symbol
mkConstCast :: (Monad m, HasCallStack) => CastOp -> Ty -> Symbol -> EdslT m Symbol
mkConstCast op t = tellConst . Unnamed t . Constant t . Const.Cast t op
truncC      t = mkConstCast CastOp.TRUNC t
zextC       t = mkConstCast CastOp.ZEXT t
sextC       t = mkConstCast CastOp.SEXT t
fpToUiC     t = mkConstCast CastOp.FPTOUI t
fpToSiC     t = mkConstCast CastOp.FPTOSI t
uiToFpC     t = mkConstCast CastOp.UITOFP t
siToFpC     t = mkConstCast CastOp.SITOFP t
fpTruncC    t = mkConstCast CastOp.FPTRUNC t
fpExtC      t = mkConstCast CastOp.FPEXT t
ptrToIntC   t s | isPtr (ty s) = mkConstCast CastOp.PTRTOINT t s
                | otherwise    = throwE . show $ text "Cannot ptr-to-int cast: " <+> pretty s <+> text "to" <+> pretty t <+> text ", symbol not a pointer!"
intToPtrC   t = mkConstCast CastOp.INTTOPTR t
bitcastC    t = mkConstCast CastOp.BITCAST t
addrSpCastC t = mkConstCast CastOp.ADDRSPACECAST t

-- ** Constant Binary Op
addC, subC, mulC, udivC, sdivC, uremC, sremC, shlC, lshrC, ashrC, andC, orC, xorC
  :: (Monad m, HasCallStack) => Symbol -> Symbol -> EdslT m Symbol
mkConstBinOp :: (Monad m, HasCallStack) => BinOp.BinOp -> Symbol -> Symbol -> EdslT m Symbol
-- TODO: verify that both are Constants!
mkConstBinOp op lhs rhs | ty lhs == ty rhs = tellConst ((\v -> Unnamed (ty v) v) (Constant (ty lhs) $ Const.BinOp op lhs rhs))
                        | otherwise = throwE . show $ text "*** Type Error:" <+> (text ("BINOP (" ++ show op ++ "), types do not agree")
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

