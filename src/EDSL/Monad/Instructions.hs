{-# OPTIONS_GHC -fprof-auto #-} 
module EDSL.Monad.Instructions
  (module EDSL.Monad.Instructions.Cast
  ,module EDSL.Monad.Instructions.Compare
  ,module EDSL.Monad.Instructions.Binary
  ,module EDSL.Monad.Instructions.Atomic
  ,module EDSL.Monad.Instructions.Constant
  ,alloca, load, store
  ,call', call, ccall, tailcall, ghccall, tailghccall
  ,ret, retVoid
  ,ubr, br, switch
  ,gep, extractValue)
  where

import Prelude hiding ((<>))

-- import EDSL.Monad.Internal
import EDSL.Monad.EdslT

import EDSL.Monad.Types
import EDSL.Monad.Instructions.Constant
import EDSL.Monad.Instructions.Cast
import EDSL.Monad.Instructions.Compare
import EDSL.Monad.Instructions.Binary
import EDSL.Monad.Instructions.Atomic

import Data.BitCode.LLVM.Classes.HasType


import Data.BitCode.LLVM.Util  (lower, isFunctionPtr, funRetTy, funParamTys, isBoolTy)
import Data.BitCode.LLVM.Value
import Data.BitCode.LLVM.Type hiding (Function)
import Data.BitCode.LLVM.Types (BasicBlockId)
import Data.BitCode.LLVM.Instruction (TailCallKind)
import Data.BitCode.LLVM.CallingConv (CallingConv)

import qualified Data.BitCode.LLVM.CallingConv     as CConv
import qualified Data.BitCode.LLVM.Instruction     as Inst

import Data.BitCode.LLVM.Pretty as P
import Text.PrettyPrint as P

import GHC.Stack (HasCallStack)
import Data.Word (Word64)
-- import Debug.Trace

-- tellInst'' x = exceptT x >>= lift . tellInst'

alloca :: (HasCallStack, Monad m) => Ty -> Symbol -> EdslT m Symbol
alloca ty size = tellInst' =<< Inst.Alloca <$> ptr ty <*> pure size <*> pure 0

load :: (HasCallStack, Monad m) => Symbol -> EdslT m Symbol
load s | isPtr (ty s) = tellInst' =<< Inst.Load <$> (deptr (ty s)) <*> pure s <*> pure 0
       | otherwise    = sthrowE $ text "Cannlot load:" <+> pretty s <+> text "must be of pointer type!"
store :: (HasCallStack, Monad m)
      => Symbol -- ^ Source
      -> Symbol -- ^ Target
      -> EdslT m ()
store target source -- = tellInst (Inst.Store target source 0) >> pure ()
  | not (isPtr (ty target))        = sthrowE $ text "can not store" <+> pretty source <+> text "in" <+> pretty target <+> text "Target" <+> pretty target <+> text "must be of ptr type."
  | lower (ty target) == ty source = tellInst (Inst.Store target source 0) >> pure ()
  | otherwise                      = sthrowE $ text "can not store " <+> pretty source <+> text "in" <+> pretty target

call' :: (HasCallStack, Monad m) => TailCallKind -> CallingConv -> Symbol -> [Symbol] -> EdslT m (Maybe Symbol)
call' tck cc f args
  | not . isFunctionPtr . ty $ f
  = sthrowE $ text "Cannot call: " <+> pretty (ty f) <+> text "must be a function pointer."
  -- TODO: length should be equal, if not vararg.
  | length args < length (funParamTys (ty f))
  = sthrowE $ text "Cannot call: " <+> (pretty f <+> text "with insufficent arguments."
                                              $+$ text "Params" <+> P.int (length (funParamTys (ty f))) <> text ":" <+> pretty (funParamTys (ty f))
                                              $+$ text "Args  " <+> P.int (length args) <> text ":" <+> pretty args)
--  | not . Prelude.and $ zipWith (==) (map ty args) (funParamTys (ty f))
--  = sthrowE $ text "Cannot call: " <+> (pretty f <+> text "text with arguments, not matching the signature."
--                                             $+$ text "Sig :" <+> pretty (funParamTys (ty f))
--                                             $+$ text "Args:" <+> pretty (map ty args))
  -- otherwise asssume function. We must not look into the function value, otherwise the lazyness
  -- required for the lazy "label" resolution is impossible.
  | otherwise = case isPtr (ty f) of
      True  -> tellInst $ Inst.Call (funRetTy (ty f)) tck cc (trace "accessing fn" f) (ty f) (trace "accessing fn args" args)
      False -> sthrowE $ text "Callee must be of ptr type. Callee: " <+> pretty f 

call :: (HasCallStack, Monad m) => CallingConv -> Symbol -> [Symbol] -> EdslT m (Maybe Symbol)
call cc f args = trace "[call]" <$> call' Inst.None cc f args
tailcall :: (HasCallStack, Monad m) => CallingConv -> Symbol -> [Symbol] -> EdslT m (Maybe Symbol)
tailcall cc f args = trace "[tailcall]" <$> call' Inst.Tail cc f args

ccall :: (HasCallStack, Monad m) => Symbol -> [Symbol] -> EdslT m (Maybe Symbol)
ccall f args = trace "[ccall]" <$> call' Inst.None CConv.C f args
ghccall :: (HasCallStack, Monad m) => Symbol -> [Symbol] -> EdslT m (Maybe Symbol)
ghccall f args = trace "[ghccall]" <$> call' Inst.None CConv.GHC f args

tailccall :: Monad m => Symbol -> [Symbol] -> EdslT m (Maybe Symbol)
tailccall f args = trace "[tailccall]" <$> call' Inst.Tail CConv.C f args
tailghccall :: Monad m => Symbol -> [Symbol] -> EdslT m (Maybe Symbol)
tailghccall f args = trace "[tailghccall]" <$> call' Inst.Tail CConv.GHC f args

ret :: Monad m => Symbol -> EdslT m (Maybe Symbol)
ret = tellInst . Inst.Ret . Just 
retVoid :: Monad m => EdslT m ()
retVoid = tellInst (Inst.Ret Nothing) >> pure ()

ubr :: Monad m => BasicBlockId -> EdslT m (Maybe Symbol)
ubr = tellInst . Inst.UBr
br :: Monad m => Symbol -> BasicBlockId -> BasicBlockId -> EdslT m () -- (Maybe Symbol)
br cond l r {- | isBoolTy cond -} = trace "[br]" <$> tellInst (Inst.Br cond l r) >> pure ()
            -- | otherwise     = sthrowE $ text "Cannot branch with " <+> pretty cond <+> text " condition. Must be i1 (Bool)"

switch :: Monad m => Symbol -> BasicBlockId -> [(Symbol, BasicBlockId)] -> EdslT m ()
switch cond def cases = trace "[switch]" <$> tellInst (Inst.Switch cond def cases) >> pure ()

gep :: Monad m => Symbol -> [Symbol] -> EdslT m Symbol
gep s ss = trace "[gep]" <$> tellInst' $ trace "[gepI]" $ Inst.Gep (ty s) True (trace "[gep] base" s) (trace "[gep] idxs" ss)

extractValue :: (HasCallStack, Monad m) => Symbol -> [Word64] -> EdslT m Symbol
extractValue s = tellInst' . Inst.ExtractValue s

