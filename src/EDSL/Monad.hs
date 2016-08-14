{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module EDSL.Monad
  (BodyBuilderT, BodyBuilder
  , execBodyBuilderT
  , execBodyBuilder
  , tellInst
  , tellInst'
  , tellNewBlock
  )
  where

import Control.Monad.Fix (MonadFix(..))

import Data.BitCode.LLVM.Types (BasicBlockId)
import Data.BitCode.LLVM.Util
import Data.BitCode.LLVM.Pretty
import Text.PrettyPrint
import Data.Maybe (fromMaybe)
import qualified Data.BitCode.LLVM.Instruction     as Inst
import qualified Data.BitCode.LLVM.Function        as Func
import qualified Data.BitCode.LLVM.Value           as Val

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Functor.Identity
import Control.Monad.Trans.Class


--------------------------------------------------------------------------------
-- Function Body Monad... or building blocks is monadic.
-- We want to be able to obtain references for later use
-- and we need to provide some form of unique symbol supply
-- the simples being a counter.

data FnCtx = FnCtx { nInst :: Int, nRef :: Int, nBlocks :: Int, blocks :: [Func.BasicBlock] } deriving Show
-- instance Monoid FnCtx where
--   mempty = FnCtx 0 0 mempty
--   (FnCtx is bs bbs) `mappend` (FnCtx is' bs' bbs') = FnCtx (is + is') (bs + bs') (bbs `mappend` shift is bs bbs')
--     where shift m n (Func.BasicBlock bi) = Func.BasicBlock $ map (shiftBB m n) bi
--           shift m n (Func.NamedBlock name bi) = Func.NamedBlock name $ map (shiftBB m n) bi
--           shiftBB :: Int -> Int -> Func.BasicBlock -> Func.BasicBlock
--           shiftBB m n = map (shiftBI m n)
--           shiftBI :: Int -> Int -> Func.BlockInst -> Func.BlockInst
--           shiftBI m n (mb, i) = (shiftS m n <$> mb, shiftI m n i)
--           shiftS m n (Val.Named name v) = (Val.Named name (shiftV m n v))
--           shiftS m n (Val.Unnamed v)    = (Val.Unnamed (shiftV m n v))
--           shiftV m n (Val.TRef t r)     = (Val.TRef t (shiftR m n r))
--           shiftV _ _ x                  = x
--           shiftR                        = (+)
-- FnCtx -> (a, FnCtx)

newtype BodyBuilderT m a = BodyBuilderT { runBodyBuilderT :: StateT FnCtx m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadFix, MonadIO)

type BodyBuilder a = BodyBuilderT Identity a

modifyCtx :: Monad m => (FnCtx -> FnCtx) -> BodyBuilderT m ()
modifyCtx = BodyBuilderT . modify
getsCtx :: Monad m => (FnCtx -> a) -> BodyBuilderT m a
getsCtx = BodyBuilderT . gets

-- mapping over blocks
execBodyBuilderT :: Monad m => Int -> BodyBuilderT m a -> m [Func.BasicBlock]
execBodyBuilderT instOffset = fmap (map (Func.bbmap reverse) . reverse . blocks . snd) . flip runStateT (FnCtx instOffset 0 0 mempty) . runBodyBuilderT

execBodyBuilder :: Int -> BodyBuilderT Identity a -> [Func.BasicBlock]
execBodyBuilder instOffset = runIdentity . execBodyBuilderT instOffset

-- | Adds an instruction to a block.
addInst :: Func.BasicBlock -> Func.BlockInst -> Func.BasicBlock
addInst (Func.BasicBlock is) i = (Func.BasicBlock (i:is))
addInst (Func.NamedBlock n is) i = (Func.NamedBlock n (i:is))

-- | Add an instruction to the current block.
-- returns @Just ref@ if the instruction retuns
-- a value. @Nothing@ if the instruction has no result.
tellInst :: Monad m => Inst.Inst -> BodyBuilderT m (Maybe Val.Symbol)
tellInst inst = do
  nr <- getsCtx nRef
  case Val.Unnamed . flip Val.TRef nr <$> instTy inst of
    ref@(Just _) -> do modifyCtx (\ctx -> ctx { nInst  = 1 + nInst ctx
                                              , nRef   = 1 + nRef ctx
                                              , blocks = let (b:bbs) = blocks ctx
                                                         in ((addInst b (ref, inst)):bbs)
                                              })
                       pure ref
    ref@Nothing  -> do modifyCtx (\ctx -> ctx { nInst = 1 + nInst ctx
                                              , blocks = let (b:bbs) = blocks ctx
                                                         in ((addInst b (ref, inst)):bbs)
                                              })
                       pure ref

-- | Add an instruction to the current block
-- and obtain it's return value.  WARN: Use this
-- only if you know the instruction returns a value.
tellInst' :: Monad m => Inst.Inst -> BodyBuilderT m Val.Symbol
tellInst' inst = tellInst inst >>= \case
  Just s -> pure s
  Nothing -> fail ("Expected instruction " ++ show inst ++ " to return a symbol!")

-- | Adds a new block to the function.
tellNewBlock :: Monad m => BodyBuilderT m BasicBlockId
tellNewBlock = do
  blockId <- getsCtx (fromIntegral . nBlocks)
  modifyCtx (\c -> c { nBlocks = (nBlocks c) + 1, blocks = (Func.BasicBlock []):blocks c})
  return blockId
