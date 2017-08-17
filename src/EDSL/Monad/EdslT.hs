{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE TupleSections #-}
module EDSL.Monad.EdslT
  (
  -- EdslT
    Error, Inst
  , EdslT, Edsl
  , runEdslT
  , serror, sthrowE, exceptT
  , tellLabel, tellGlobal, tellType, tellConst
  , tellInst, tellInst'
  )
  where

import qualified EDSL.Monad.Internal as M
import qualified Data.BitCode.LLVM.Instruction as Inst (Inst)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), throwE, runExceptT)
import Data.Functor.Identity (Identity)
import Data.BitCode.LLVM.Pretty (pretty)
import Data.BitCode.LLVM.Value (Symbol)
import Data.BitCode.LLVM.Type  (Ty)

type Error = String
type Inst = Either Error Inst.Inst

type EdslT m a = ExceptT Error (M.BodyBuilderT m) a
type Edsl a = ExceptT Error (M.BodyBuilderT Identity) a

--      runExceptT :: ExceptT e m a -> m (Either e a)
-- runBodyBuilderT :: Int -> BodyBuilderT m a -> m (a, BodyBuilderResult)

runEdslT :: Monad m => Int -> EdslT m a -> m (Either Error (a, M.BodyBuilderResult))
runEdslT i = fmap (\(a, b) -> fmap (,b) a) . M.runBodyBuilderT i . runExceptT

serror :: (Show a) => a -> Inst
serror = Left . show

exceptT :: Monad m => Either Error a -> EdslT m a
exceptT res = case res of
  r@(Right{}) -> ExceptT (pure r)
  (Left errMsg) -> do
    log <- lift M.askLog
    insts <- unlines . fmap (('\t':) . show . pretty) <$> lift (M.askInsts 20)
    ExceptT (pure . Left $ unlines ["LOG", log, "Last 20 instructions", insts, "ERROR", errMsg])

sthrowE :: (Monad m, Show a) => a -> ExceptT Error m b
sthrowE = throwE . show

tellLabel, tellConst, tellGlobal :: (Monad m) => Symbol -> EdslT m Symbol
tellLabel = lift . M.tellLabel
tellConst = lift . M.tellConst
tellGlobal = lift . M.tellGlobal
tellType :: Monad m => Ty -> EdslT m Ty
tellType = lift . M.tellType

tellInst :: Monad m => Inst.Inst -> EdslT m (Maybe Symbol) 
tellInst = lift . M.tellInst
tellInst' :: Monad m => Inst.Inst -> EdslT m Symbol
tellInst' = lift . M.tellInst'
