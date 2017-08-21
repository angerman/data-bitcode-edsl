{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module EDSL.Monad.Internal
  (BodyBuilderT, BodyBuilder, BodyBuilderResult(..), Resolver
  , runBodyBuilderT
  , execBodyBuilderT
  , execBodyBuilder
  , evalBodyBuilderT
  , evalBodyBuilder
  , tellInst
  , tellInst'
  , tellNewBlock, askBlocks, takeBlocks
  -- * Labels, Globals, Constant and Types
  , tellLabel, askLabels
  , tellGlobal, askGlobals
  , tellConst, askConsts
  , tellType, askTypes
  -- * Debugging
  , askInsts
  , tellLog
  , tellLogShow
  , askLog
  )
  where

import Control.Monad.Fix (MonadFix(..))

import Data.BitCode.LLVM.Types (BasicBlockId)
import Data.BitCode.LLVM.Util
import Data.BitCode.LLVM.Pretty
import Data.BitCode.LLVM.Classes.HasType
import Text.PrettyPrint
import Data.Maybe (fromMaybe)
import qualified Data.BitCode.LLVM.Instruction     as Inst
import qualified Data.BitCode.LLVM.Function        as Func
import qualified Data.BitCode.LLVM.Value           as Val
import qualified Data.BitCode.LLVM.Type            as Ty
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Functor.Identity
import Control.Monad.Trans.Class

--------------------------------------------------------------------------------
-- Function Body Monad... or building blocks is monadic.
-- We want to be able to obtain references for later use
-- and we need to provide some form of unique symbol supply
-- the simples being a counter.

type Labels = Set (String, Ty.Ty)
type Consts = Set Val.Symbol
type Globals = Set Val.Symbol
type Types = Set Ty.Ty

type Resolver = String -> Val.Symbol

data FnCtx = FnCtx
  { nInst :: Int
  , nRef :: Int
  , nBlocks :: Int
  , blocks :: [Func.BasicBlock]
  -- extracing globals and labels
  -- after building them up from the
  -- function blocks is too expensive.
  -- Therefore we collect Globals and
  -- Labels during the construction.
  , globals :: Globals
  , consts :: Consts
  , labels :: Labels
  , types :: Types
  -- logging facilities
  , _log :: [String]
  , _insts :: [Inst.Inst]
  -- label resolver
  , resolver :: Resolver
  }

mkCtx i r = FnCtx i 0 0 mempty Set.empty Set.empty Set.empty Set.empty mempty mempty r
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

newtype BodyBuilderT m a = BodyBuilderT { unBodyBuilderT :: StateT FnCtx m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadFix, MonadIO)

data BodyBuilderResult = BBR
  { _blocks  :: [Func.BasicBlock]
  , _globals :: Globals
  , _consts  :: Consts
  , _labels  :: Labels
  , _types   :: Types
  }
  deriving (Show)

type BodyBuilder a = BodyBuilderT Identity a

modifyCtx :: Monad m => (FnCtx -> FnCtx) -> BodyBuilderT m ()
modifyCtx = BodyBuilderT . modify
getsCtx :: Monad m => (FnCtx -> a) -> BodyBuilderT m a
getsCtx = BodyBuilderT . gets

-- mapping over blocks
runBodyBuilderT :: Monad m => Resolver -> Int -> BodyBuilderT m a -> m (a, BodyBuilderResult)
runBodyBuilderT resolver instOffset = fmap mkResult . flip runStateT (mkCtx instOffset resolver) . unBodyBuilderT
  where
    mkResult :: (a, FnCtx) -> (a, BodyBuilderResult)
    mkResult (a, s) = (a, BBR (reverseBlocks s) (globals s) (consts s) (labels s) (types s))
    reverseBlocks :: FnCtx -> [Func.BasicBlock]
    reverseBlocks = map (Func.bbmap reverse) . reverse . blocks

execBodyBuilderT :: Monad m => Resolver -> Int -> BodyBuilderT m a -> m BodyBuilderResult
execBodyBuilderT resolver instOffset = fmap snd . runBodyBuilderT resolver instOffset

execBodyBuilder :: Resolver -> Int -> BodyBuilderT Identity a -> BodyBuilderResult
execBodyBuilder resolver instOffset = runIdentity . execBodyBuilderT resolver instOffset

evalBodyBuilderT :: Monad m => Resolver -> Int -> BodyBuilderT m a -> m a
evalBodyBuilderT resolver instOffset = flip evalStateT (mkCtx instOffset resolver) . unBodyBuilderT

evalBodyBuilder :: Resolver -> Int -> BodyBuilderT Identity a -> a
evalBodyBuilder resolver instOffset = runIdentity . evalBodyBuilderT resolver instOffset


-- | Adds an instruction to a block.
addInst :: Func.BasicBlock -> Func.BlockInst -> Func.BasicBlock
addInst (Func.BasicBlock is) i = (Func.BasicBlock (i:is))
addInst (Func.NamedBlock n is) i = (Func.NamedBlock n (i:is))

tellLog :: Monad m => String -> BodyBuilderT m ()
tellLog l = modifyCtx (\c -> c { _log = l:(_log c) })

tellLogShow :: (Monad m, Show s) => s -> BodyBuilderT m () 
tellLogShow = tellLog . show

askLog :: Monad m => BodyBuilderT m String
askLog = unlines . fmap ('\t':) . reverse <$> getsCtx _log

askInsts :: Monad m => Int -> BodyBuilderT m [Func.BlockInst]
askInsts n = do
  bbs <- getsCtx blocks
  if (null bbs)
    then return []
    else return . reverse . take n . blockInsts . head $ bbs 
  where
    blockInsts :: Func.BasicBlock -> [Func.BlockInst]
    blockInsts (Func.BasicBlock is) = is
    blockInsts (Func.NamedBlock _ is) = is


tellLabel :: Monad m => String -> Ty.Ty -> BodyBuilderT m Val.Symbol
tellLabel name t = do
  modifyCtx (\ctx -> ctx { labels = Set.insert (name, t) (labels ctx) })
  res <- getsCtx resolver
  return $ res name

askLabels :: Monad m => BodyBuilderT m Labels
askLabels = getsCtx labels

tellGlobal :: Monad m => Val.Symbol -> BodyBuilderT m Val.Symbol
tellGlobal g = do
  modifyCtx (\ctx -> ctx { globals = Set.insert g (globals ctx) })
  return g

askGlobals :: Monad m => BodyBuilderT m Globals
askGlobals = getsCtx globals

tellConst :: Monad m => Val.Symbol -> BodyBuilderT m Val.Symbol
tellConst c = do
  modifyCtx (\ctx -> ctx { consts = Set.insert c (consts ctx) })
  return c

askConsts :: Monad m => BodyBuilderT m Consts
askConsts = getsCtx consts

tellType :: Monad m => Ty.Ty -> BodyBuilderT m Ty.Ty
tellType t = do
  modifyCtx (\ctx -> ctx { types = Set.insert t (types ctx) })
  return t

askTypes :: Monad m => BodyBuilderT m Types
askTypes = getsCtx types

-- | Add an instruction to the current block.
-- returns @Just ref@ if the instruction retuns
-- a value. @Nothing@ if the instruction has no result.
tellInst :: Monad m => Inst.Inst -> BodyBuilderT m (Maybe Val.Symbol)
tellInst inst = do

  -- [TODO], this adds extra over hread, remove it
  --         and use the blocks directry to obtain
  --         the instructions
  modifyCtx (\c -> c { _insts = inst:(_insts c) })
  
  nr <- getsCtx nRef
  case Val.Unnamed . flip Val.TRef nr <$> instTy inst of
    ref@(Just s) -> do tellType (ty s) -- record the instruction type
                       modifyCtx (\ctx -> ctx { nInst  = 1 + nInst ctx
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

askBlocks :: Monad m => BodyBuilderT m [Func.BasicBlock]
askBlocks = reverseBlocks <$> getsCtx blocks
  where
    reverseBlocks :: [Func.BasicBlock] -> [Func.BasicBlock]
    reverseBlocks = map (Func.bbmap reverse) . reverse

takeBlocks :: Monad m => Int -> BodyBuilderT m [Func.BasicBlock]
takeBlocks i = do
  blocks <- askBlocks
  modifyCtx (\ctx -> ctx { nInst = i
                         , nRef =0
                         , nBlocks = 0
                         , blocks = mempty
                         })
  return blocks
