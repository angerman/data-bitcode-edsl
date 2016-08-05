{-# LANGUAGE LambdaCase #-}
module EDSL.Monad
  (BodyBuilder
  , execBodyBuilder
  , tellInst
  , tellInst'
  , tellNewBlock
  )
  where

import Control.Monad.Fix (MonadFix(..))

import Data.BitCode.LLVM.Types (BasicBlockId)
import qualified Data.BitCode.LLVM.Instruction     as Inst
import qualified Data.BitCode.LLVM.Function        as Func
import qualified Data.BitCode.LLVM.Value           as Val

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

newtype BodyBuilder a = BodyBuilder { runBodyBuilder :: FnCtx -> (a, FnCtx) }

-- mapping over blocks
execBodyBuilder :: Int -> BodyBuilder a -> [Func.BasicBlock]
execBodyBuilder instOffset = map (Func.bbmap reverse) . reverse . blocks . snd . flip runBodyBuilder (FnCtx instOffset 0 0 mempty)

instance Functor BodyBuilder where
  fmap f b = BodyBuilder $ \c -> let (x, s) = runBodyBuilder b c in (f x, s)

instance Applicative BodyBuilder where
  pure x = BodyBuilder $ \c -> (x, c)
  a <*> b = error $ "No applicative interface!"

instance Monad BodyBuilder where
  m >>= k = BodyBuilder $ \s -> let (x, s') = runBodyBuilder m s
                                in runBodyBuilder (k x) s'

instance MonadFix BodyBuilder where
  mfix f = BodyBuilder $ \c -> let (x, s') = runBodyBuilder (f x) c
                               in (x, s')

-- | Adds an instruction to a block.
addInst :: Func.BasicBlock -> Func.BlockInst -> Func.BasicBlock
addInst (Func.BasicBlock is) i = (Func.BasicBlock (i:is))
addInst (Func.NamedBlock n is) i = (Func.NamedBlock n (i:is))

-- | Add an instruction to the current block.
-- returns @Just ref@ if the instruction retuns
-- a value. @Nothing@ if the instruction has no result.
tellInst :: Inst.Inst -> BodyBuilder (Maybe Val.Symbol)
tellInst inst = BodyBuilder $ \(FnCtx ni nr nb (b:bbs)) -> case Val.Unnamed . flip Val.TRef nr <$> Inst.instTy inst of
                                                             ref@(Just _) -> (ref, FnCtx (ni + 1) (nr + 1) nb ((addInst b (ref, inst)):bbs))
                                                             ref@Nothing  -> (ref, FnCtx (ni + 1) nr       nb ((addInst b (ref, inst)):bbs))

-- | Add an instruction to the current block
-- and obtain it's return value.  WARN: Use this
-- only if you know the instruction returns a value.
tellInst' :: Inst.Inst -> BodyBuilder Val.Symbol
tellInst' inst = tellInst inst >>= \case
  Just s -> pure s
  Nothing -> fail ("Expected instruction " ++ show inst ++ " to return a symbol!")

-- | Adds a new block to the function.
tellNewBlock :: BodyBuilder BasicBlockId
tellNewBlock = BodyBuilder $ \(FnCtx ni nr nb bbs) -> (fromIntegral nb, FnCtx ni nr (nb + 1) ((Func.BasicBlock []):bbs))
