module EDSL.Monad.Instructions.Cast where

import EDSL.Monad.EdslT

import Data.BitCode.LLVM.Opcodes.Cast

import Data.BitCode.LLVM.Classes.HasType
import Data.BitCode.LLVM.Value (Symbol)
import Data.BitCode.LLVM.Type (Ty)
import Data.BitCode.LLVM.Util
import qualified Data.BitCode.LLVM.Instruction as Inst

import Data.BitCode.LLVM.Pretty
import Text.PrettyPrint

import GHC.Stack (HasCallStack)

import Debug.Trace

-- ** Cast
trunc, zext, sext, fpToUi, fpToSi, uiToFp, siToFp, fpTrunc, fpExt, ptrToInt, intToPtr, bitcast, addrSpCast
  :: (HasCallStack, Monad m) => Ty -> Symbol -> EdslT m Symbol
mkCast :: (HasCallStack, Monad m) => CastOp -> Ty -> Symbol -> EdslT m Symbol
mkCast op t = tellInst' . Inst.Cast t op 

trunc t = mkCast TRUNC t
zext t s | ty s /= t = mkCast ZEXT t s
         -- LLVM will complain if this kind of cast appears in the bitcode; it considers the module broken.
         | otherwise = sthrowE $ text "Cannot zero extend cast:" <+> pretty s <+> text "to" <+> pretty t <> text "."
sext    t = mkCast SEXT t
fpToUi  t = mkCast FPTOUI t
fpToSi  t = mkCast FPTOSI t
uiToFp  t = mkCast UITOFP t
siToFp  t = mkCast SITOFP t
fpTrunc t = mkCast FPTRUNC t
fpExt   t = mkCast FPEXT t
ptrToInt t = mkCast PTRTOINT t
-- ptrToInt t s | isPtr (ty s) = mkCast PTRTOINT t s
--              | otherwise    = sthrowE $ text "Cannot ptr-to-int cast:" <+> pretty s <+> text "to" <+> pretty t <> text ", symbol not a pointer!"
intToPtr t = mkCast INTTOPTR t
-- intToPtr t s | isInt (ty s) = mkCast INTTOPTR t s
--             | otherwise    = sthrowE $ text "Cannot int-to-ptr cast:" <+> pretty s <+> text "to" <+> pretty t <> text ", symbol not an integer!"
bitcast t = mkCast BITCAST  t
addrSpCast t = mkCast ADDRSPACECAST t
