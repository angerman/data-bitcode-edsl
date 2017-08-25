{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
module EDSL.Monad.EdslT
  (
    M.BodyBuilderT
  -- EdslT
  , Error, Inst
  , EdslT, Edsl
  , runEdslT, evalEdslT, evalEdsl
  , serror, sthrowE, exceptT
  , tellFunc, tellDecl
  , tellLabel, tellGlobal, tellGlobal', tellType, tellConst
  , tellInst, tellInst'
  , askBlocks, askLabels, askConsts, askGlobals, askTypes
  , withResolver
  )
  where

import qualified EDSL.Monad.Internal as M
import qualified Data.BitCode.LLVM.Instruction as Inst (Inst)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), throwE, runExceptT)
import Control.Monad.Fix (MonadFix)
import Data.Functor.Identity (Identity, runIdentity)
import Data.BitCode.LLVM.Pretty (pretty)
import Data.BitCode.LLVM.Value (Value, Symbol, trace, traceM)
import Data.BitCode.LLVM.Type  (Ty)
import Data.BitCode.LLVM.Function (BasicBlock)

import Data.Set (Set)

import GHC.Stack (HasCallStack)

type Error = String
type Inst = Either Error Inst.Inst

type EdslT m a = ExceptT Error (M.BodyBuilderT m) a
type Edsl a = ExceptT Error (M.BodyBuilderT Identity) a


--      runExceptT :: ExceptT e m a -> m (Either e a)
-- runBodyBuilderT :: Int -> BodyBuilderT m a -> m (a, BodyBuilderResult)

withResolver :: (HasCallStack, Monad m, MonadFix m) => EdslT m a -> EdslT m a
withResolver a = mdo
  lift (M.setResolver r)
  a' <- a
  r <- lift M.buildResolver
  return a'

runEdslT :: (HasCallStack, Monad m, MonadFix m) => Int -> EdslT m a -> m (Either Error (a, M.BodyBuilderResult))
runEdslT i = fmap (\(a, b) -> fmap (,b) a) . M.runBodyBuilderT i . runExceptT . withResolver

evalEdslT :: (HasCallStack, Monad m, MonadFix m) => Int -> EdslT m a -> m (Either Error a)
evalEdslT i = M.evalBodyBuilderT i . runExceptT . withResolver

evalEdsl :: HasCallStack => Int -> Edsl a -> Either Error a
evalEdsl i = runIdentity . evalEdslT i 

serror :: (HasCallStack, Show a) => a -> Inst
serror = Left . show

exceptT :: (HasCallStack, Monad m) => Either Error a -> EdslT m a
exceptT res = case res of
  r@(Right{}) -> ExceptT (pure r)
  (Left errMsg) -> do
    log <- lift M.askLog
    insts <- unlines . fmap (('\t':) . show . pretty) <$> lift (M.askInsts 20)
    ExceptT (pure . Left $ unlines ["LOG", log, "Last 20 instructions", insts, "ERROR", errMsg])

sthrowE :: (Monad m, Show a) => a -> ExceptT Error m b
sthrowE = throwE . show

tellLabel :: (HasCallStack, Monad m) => String -> Ty -> Maybe Value -> EdslT m Symbol
tellLabel name ty = lift . M.tellLabel name ty
tellConst, tellGlobal, tellGlobal', tellFunc, tellDecl :: (HasCallStack, Monad m) => Symbol -> EdslT m Symbol
tellConst = lift . M.tellConst
tellGlobal = lift . M.tellGlobal
tellGlobal' = lift . M.tellGlobal'
tellFunc = lift . M.tellFunc
tellDecl = lift . M.tellDecl

tellType :: Monad m => Ty -> EdslT m Ty
tellType = lift . M.tellType

tellInst :: Monad m => Inst.Inst -> EdslT m (Maybe Symbol) 
tellInst = lift . M.tellInst
tellInst' :: Monad m => Inst.Inst -> EdslT m Symbol
tellInst' = lift . M.tellInst'

-- [TODO]: newtype?
askBlocks :: Monad m => EdslT m [BasicBlock]
askBlocks = lift M.askBlocks

askLabels :: Monad m => EdslT m M.Labels
askLabels = lift M.askLabels
askConsts, askGlobals :: Monad m => EdslT m M.Consts
askConsts = lift M.askConsts
askGlobals = lift M.askGlobals
askTypes :: Monad m => EdslT m M.Types
askTypes = lift M.askTypes
