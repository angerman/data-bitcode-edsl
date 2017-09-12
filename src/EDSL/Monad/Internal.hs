{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns, CPP #-}
module EDSL.Monad.Internal
  (BodyBuilderT, BodyBuilder, BodyBuilderResult(..), Resolver
  , Labels, Consts, Types
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
  , tellFunc, tellDecl
  , tellGlobal', tellGlobal, askGlobals
  , tellConst, askConsts
  , tellType, askTypes, askTypeList
  , askDecls, askFuncs
  , buildResolver, setResolver
  , Offset
  , setGOffset, setDOffset, setFOffset, setCOffset
  -- * Debugging
  , askInsts
  , tellLog
  , tellLogShow
  , askLog
  )
  where

import Control.Monad.Fix (MonadFix(..))

import EDSL.Monad.Default

import Data.BitCode.LLVM.Types (BasicBlockId)
import Data.BitCode.LLVM.Util
import Data.BitCode.LLVM.Pretty
import Data.BitCode.LLVM.Classes.HasType
import Text.PrettyPrint
import Data.Maybe (fromMaybe)
import qualified Data.BitCode.LLVM.Instruction     as Inst
import qualified Data.BitCode.LLVM.Function        as Fn
import qualified Data.BitCode.LLVM.Value           as Val
import qualified Data.BitCode.LLVM.Type            as Ty
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.List (sortBy)
import Data.Function (on)

import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Functor.Identity
import Control.Monad.Trans.Class

import qualified Debug.Trace as Trace

import GHC.Stack (HasCallStack)

import Data.Word (Word64)

--import qualified Data.Set.Internal as SetI
import Data.Set.Internal (Set(Bin, Tip))
import EDSL.Monad.PtrEquality (ptrEq)
#if __GLASGOW_HASKELL__
import GHC.Exts ( build, lazy )
#endif

import qualified System.Environment as Env (lookupEnv)
import Data.Maybe (isJust)
import System.IO.Unsafe (unsafePerformIO)

trace :: String -> a -> a
trace _ x = x
-- trace = Trace.trace
-- traceM :: Applicative f => String -> f () 
-- traceM = Trace.traceM

delta,ratio :: Int
delta = 3
ratio = 2

balanceL :: a -> Set a -> Set a -> Set a
balanceL x l r = case r of
  Tip -> case l of
           Tip -> Bin 1 x Tip Tip
           (Bin _ _ Tip Tip) -> Bin 2 x l Tip
           (Bin _ lx Tip (Bin _ lrx _ _)) -> Bin 3 lrx (Bin 1 lx Tip Tip) (Bin 1 x Tip Tip)
           (Bin _ lx ll@(Bin _ _ _ _) Tip) -> Bin 3 lx ll (Bin 1 x Tip Tip)
           (Bin ls lx ll@(Bin lls _ _ _) lr@(Bin lrs lrx lrl lrr))
             | lrs < ratio*lls -> Bin (1+ls) lx ll (Bin (1+lrs) x lr Tip)
             | otherwise -> Bin (1+ls) lrx (Bin (1+lls+Set.size lrl) lx ll lrl) (Bin (1+Set.size lrr) x lrr Tip)

  (Bin rs _ _ _) -> case l of
           Tip -> Bin (1+rs) x Tip r

           (Bin ls lx ll lr)
              | ls > delta*rs  -> case (ll, lr) of
                   (Bin lls _ _ _, Bin lrs lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lx ll (Bin (1+rs+lrs) x lr r)
                     | otherwise -> Bin (1+ls+rs) lrx (Bin (1+lls+Set.size lrl) lx ll lrl) (Bin (1+rs+Set.size lrr) x lrr r)
                   (_, _) -> error "Failure in Data.Map.balanceL"
              | otherwise -> Bin (1+ls+rs) x l r
{-# NOINLINE balanceL #-}

-- balanceR is called when right subtree might have been inserted to or when
-- left subtree might have been deleted from.
balanceR :: a -> Set a -> Set a -> Set a
balanceR x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 x Tip Tip
           (Bin _ _ Tip Tip) -> Bin 2 x Tip r
           (Bin _ rx Tip rr@(Bin _ _ _ _)) -> Bin 3 rx (Bin 1 x Tip Tip) rr
           (Bin _ rx (Bin _ rlx _ _) Tip) -> Bin 3 rlx (Bin 1 x Tip Tip) (Bin 1 rx Tip Tip)
           (Bin rs rx rl@(Bin rls rlx rll rlr) rr@(Bin rrs _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rx (Bin (1+rls) x Tip rl) rr
             | otherwise -> Bin (1+rs) rlx (Bin (1+Set.size rll) x Tip rll) (Bin (1+rrs+Set.size rlr) rx rlr rr)

  (Bin ls _ _ _) -> case r of
           Tip -> Bin (1+ls) x l Tip

           (Bin rs rx rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (Bin rls rlx rll rlr, Bin rrs _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rx (Bin (1+ls+rls) x l rl) rr
                     | otherwise -> Bin (1+ls+rs) rlx (Bin (1+ls+Set.size rll) x l rll) (Bin (1+rrs+Set.size rlr) rx rlr rr)
                   (_, _) -> error "Failure in Data.Map.balanceR"
              | otherwise -> Bin (1+ls+rs) x l r
{-# NOINLINE balanceR #-}

insert' :: Ord a => a -> Set a -> (a, Set a)
insert' x0 = go x0 x0
  where
    go :: Ord a => a -> a -> Set a -> (a, Set a)
    go orig !_ Tip = (orig, Set.singleton (lazy orig))
    go orig !x t@(Bin sz y l r) = case compare x y of
        LT | l' `ptrEq` l -> (y, t)
           | otherwise -> (orig', balanceL y l' r)
           where (orig', !l') = go orig x l
        GT | r' `ptrEq` r -> (y, t)
           | otherwise -> (orig', balanceR y l r')
           where (orig', !r') = go orig x r
        EQ -> (y, t)
#if __GLASGOW_HASKELL__
{-# INLINABLE insert' #-}
#else
{-# INLINE insert' #-}
#endif


--------------------------------------------------------------------------------
-- Function Body Monad... or building blocks is monadic.
-- We want to be able to obtain references for later use
-- and we need to provide some form of unique symbol supply
-- the simples being a counter.

newtype Label = Label (String, Ty.Ty, Maybe Val.Value) deriving (Ord, Eq, Show)

data IndexedType = ITy { _tIdx :: Int, _tTy :: Ty.Ty } deriving (Show)
instance Eq IndexedType where
  (ITy _ lhs) == (ITy _ rhs) = lhs == rhs
instance Ord IndexedType where
  (ITy _ lhs) `compare` (ITy _ rhs) = lhs `compare` rhs

type Labels = Set Label
type Consts = Set Val.Symbol
type Globals = Set Val.Symbol
type Funcs  = Set Val.Symbol
type Decls  = Set Val.Symbol
type Types = Set IndexedType

type Resolver = String -> Val.Symbol

{-# NOINLINE llvmDebugResolverFlag #-}
llvmDebugResolverFlag :: Bool
llvmDebugResolverFlag = unsafePerformIO $ isJust <$> Env.lookupEnv "LLVM_DEBUG_RESOLVER"

data FnCtx = FnCtx
  { nInst :: Int
  , nRef :: Int
  , nBlocks :: Int
  , blocks :: [Fn.BasicBlock]
  -- extracing globals and labels
  -- after building them up from the
  -- function blocks is too expensive.
  -- Therefore we collect Globals and
  -- Labels during the construction.
  , globals :: Globals
  , funcs  :: Funcs
  , decls  :: Decls
  , consts :: Consts
  , labels :: Labels
  , types :: Types
  -- logging facilities
  , _log :: [String]
  , _insts :: [Inst.Inst]
  -- label resolver
  , resolver :: Resolver
  , gOffset :: Word64 -> Word64
  , dOffset :: Word64 -> Word64
  , fOffset :: Word64 -> Word64
  , cOffset :: Word64 -> Word64
  }

mkCtx i = FnCtx i 0 0 mempty Set.empty Set.empty Set.empty Set.empty Set.empty Set.empty mempty mempty
          (error "no resolver") (error "no gOffset") (error "no dOffset") (error "no fOffset") (error "no cOffset")
-- instance Monoid FnCtx where
--   mempty = FnCtx 0 0 mempty
--   (FnCtx is bs bbs) `mappend` (FnCtx is' bs' bbs') = FnCtx (is + is') (bs + bs') (bbs `mappend` shift is bs bbs')
--     where shift m n (Fn.BasicBlock bi) = Fn.BasicBlock $ map (shiftBB m n) bi
--           shift m n (Fn.NamedBlock name bi) = Fn.NamedBlock name $ map (shiftBB m n) bi
--           shiftBB :: Int -> Int -> Fn.BasicBlock -> Fn.BasicBlock
--           shiftBB m n = map (shiftBI m n)
--           shiftBI :: Int -> Int -> Fn.BlockInst -> Fn.BlockInst
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
  { _blocks  :: [Fn.BasicBlock]
  , _globals :: Globals
  , _consts  :: Consts
  , _labels  :: Labels
  , _types   :: Types
  }
  deriving (Show)

type BodyBuilder a = BodyBuilderT Identity a

modifyCtx :: (HasCallStack, Monad m) => (FnCtx -> FnCtx) -> BodyBuilderT m ()
modifyCtx = BodyBuilderT . modify
getsCtx :: (HasCallStack, Monad m) => (FnCtx -> a) -> BodyBuilderT m a
getsCtx = BodyBuilderT . gets

-- mapping over blocks
runBodyBuilderT :: (HasCallStack, Monad m) => Int -> BodyBuilderT m a -> m (a, BodyBuilderResult)
runBodyBuilderT instOffset = fmap mkResult . flip runStateT (mkCtx instOffset) . unBodyBuilderT
  where
    mkResult :: (a, FnCtx) -> (a, BodyBuilderResult)
    mkResult (a, s) = (a, BBR (reverseBlocks s) (globals s) (consts s) (labels s) (types s))
    reverseBlocks :: FnCtx -> [Fn.BasicBlock]
    reverseBlocks = map (Fn.bbmap reverse) . reverse . blocks

execBodyBuilderT :: (HasCallStack, Monad m) => Int -> BodyBuilderT m a -> m BodyBuilderResult
execBodyBuilderT instOffset = fmap snd . runBodyBuilderT instOffset

execBodyBuilder :: HasCallStack => Int -> BodyBuilderT Identity a -> BodyBuilderResult
execBodyBuilder instOffset = runIdentity . execBodyBuilderT instOffset

evalBodyBuilderT :: (HasCallStack, Monad m) => Int -> BodyBuilderT m a -> m a
evalBodyBuilderT instOffset = flip evalStateT (mkCtx instOffset) . unBodyBuilderT

evalBodyBuilder :: HasCallStack => Int -> BodyBuilderT Identity a -> a
evalBodyBuilder instOffset = runIdentity . evalBodyBuilderT instOffset


-- | Adds an instruction to a block.
addInst :: HasCallStack => Fn.BasicBlock -> Fn.BlockInst -> Fn.BasicBlock
addInst (Fn.BasicBlock is) i = (Fn.BasicBlock (i:is))
addInst (Fn.NamedBlock n is) i = (Fn.NamedBlock n (i:is))

tellLog :: (HasCallStack, Monad m) => String -> BodyBuilderT m ()
tellLog l = modifyCtx (\c -> c { _log = l:(_log c) })

tellLogShow :: (Monad m, Show s) => s -> BodyBuilderT m () 
tellLogShow = tellLog . show

askLog :: (HasCallStack, Monad m) => BodyBuilderT m String
askLog = unlines . fmap ('\t':) . reverse <$> getsCtx _log

askInsts :: (HasCallStack, Monad m) => Int -> BodyBuilderT m [Fn.BlockInst]
askInsts n = do
  bbs <- getsCtx blocks
  if (null bbs)
    then return []
    else return . reverse . take n . blockInsts . head $ bbs 
  where
    blockInsts :: Fn.BasicBlock -> [Fn.BlockInst]
    blockInsts (Fn.BasicBlock is) = is
    blockInsts (Fn.NamedBlock _ is) = is


tellLabel :: (HasCallStack, Monad m) => String -> Ty.Ty -> Maybe Val.Value -> BodyBuilderT m Val.Symbol
tellLabel name t mbVal = do
  modifyCtx (\ctx -> ctx { labels = Set.insert (Label (name, t, mbVal)) (labels ctx) })
  resolve <- getsCtx resolver
  return $ Val.Lazy name t (const (resolve name))

askLabels :: (HasCallStack, Monad m) => BodyBuilderT m Labels
askLabels = getsCtx labels

tellGlobal :: (HasCallStack, Monad m) => Val.Symbol -> BodyBuilderT m Val.Symbol
tellGlobal g = case Val.symbolValue g of
  Val.Function{} -> tellFunction g
  Val.Global{}   -> tellGlobal'  g
  Val.Constant{} -> tellConst    g

tellFunction :: (HasCallStack, Monad m) => Val.Symbol -> BodyBuilderT m Val.Symbol
tellFunction f = case Val.feProto (Val.fExtra (Val.symbolValue f)) of
  True  -> tellDecl f
  False -> tellFunc f

tellDecl :: (HasCallStack, Monad m) => Val.Symbol -> BodyBuilderT m Val.Symbol
tellDecl f = do
  decls  <- askDecls
  offset <- getsCtx dOffset
  let i = fromIntegral $ Set.size decls
      (f', decls') = insert' (Val.withIndex (\_ -> offset i) f) decls
  modifyCtx (\ctx -> ctx { decls = decls' })
  return f'

askDecls :: (HasCallStack, Monad m) => BodyBuilderT m Decls
askDecls = getsCtx decls

tellFunc :: (HasCallStack, Monad m) => Val.Symbol -> BodyBuilderT m Val.Symbol
tellFunc f = do
  funcs <- askFuncs
  offset <- getsCtx fOffset
  let i = fromIntegral $ Set.size funcs
      (f', funcs') = insert' (Val.withIndex (\_ -> offset i) f) funcs
  modifyCtx (\ctx -> ctx { funcs = funcs' })
  return f'

askFuncs :: (HasCallStack, Monad m) => BodyBuilderT m Funcs
askFuncs = getsCtx funcs

tellGlobal' :: (HasCallStack, Monad m) => Val.Symbol -> BodyBuilderT m Val.Symbol
tellGlobal' g = do
  globals <- askGlobals
  offset  <- getsCtx gOffset
  let i = fromIntegral $ Set.size globals
      (g',globals') = insert' (Val.withIndex (\_ -> offset i) g) globals
  modifyCtx (\ctx -> ctx { globals = globals' })
  return g'

askGlobals :: (HasCallStack, Monad m) => BodyBuilderT m Globals
askGlobals = getsCtx globals

tellConst :: (HasCallStack, Monad m) => Val.Symbol -> BodyBuilderT m Val.Symbol
tellConst c = do
  consts <- askConsts
  offset <- getsCtx cOffset
  let i = fromIntegral $ Set.size consts
      (c', consts') = insert' (Val.withIndex (\_ -> offset i) c) consts
  modifyCtx (\ctx -> ctx { consts = consts' })
  return c'

askConsts :: (HasCallStack, Monad m) => BodyBuilderT m Consts
askConsts = getsCtx consts

tellType :: (HasCallStack, Monad m) => Ty.Ty -> BodyBuilderT m Ty.Ty
tellType t = do
  types <- askTypes
  let i = Set.size types
      (ITy _ t', types') = insert' (ITy i t) types
  modifyCtx (\ctx -> ctx { types = types' })
  return t'

askTypes :: (HasCallStack, Monad m) => BodyBuilderT m Types
askTypes = getsCtx types

askTypeList :: (HasCallStack, Monad m) => BodyBuilderT m [Ty.Ty]
askTypeList = map _tTy . sortBy (compare `on` _tIdx) . Set.toList <$> askTypes

instName :: HasCallStack => Inst.Inst -> String
instName (Inst.BinOp _ _ _ _ _) = "BinOp"
instName (Inst.Cast _t _op _s)      = "Cast (" ++ show _op ++ ")"
instName (Inst.Alloca _ _ _)    = "Alloca"
instName (Inst.Load _ _ _)      = "Load"
instName (Inst.Store _ _ _)     = "Store"
instName (Inst.Call{})          = "Call"
instName (Inst.Cmp2 _ _ _ _)    = "Cmp2"
instName (Inst.Gep _ _ _ _)     = "Gep"
instName (Inst.ExtractValue _ _) = "ExtractValue"
instName (Inst.Ret _)           = "Ret"
instName (Inst.UBr _)           = "Ubr"
instName (Inst.Br _ _ _)        = "Br"
instName (Inst.Switch _ _ _)    = "Switch"
instName (Inst.Fence _ _)       = "Fence"
instName (Inst.CmpXchg _ _ _ _ _ _) = "CmpXchg"
instName (Inst.AtomicRMW _ _ _ _ _) = "AtomicRMW"
instName (Inst.AtomicStore _ _ _ _ _) = "Atomic Store"
instName (Inst.AtomicLoad _ _ _ _ _) = "Atomic Load"

-- | Add an instruction to the current block.
-- returns @Just ref@ if the instruction retuns
-- a value. @Nothing@ if the instruction has no result.
tellInst :: (HasCallStack, Monad m) => Inst.Inst -> BodyBuilderT m (Maybe Val.Symbol)
tellInst inst = do

  -- [TODO], this adds extra over hread, remove it
  --         and use the blocks directry to obtain
  --         the instructions
  modifyCtx (\c -> c { _insts = inst:(_insts c) })
  
  nr <- getsCtx nRef
  case (\t ->   Val.withInstIndex (const (fromIntegral nr))
              . Val.mkUnnamedInst t
              $ Val.TRef t nr) <$> instTy inst of
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
tellInst' :: (HasCallStack, Monad m) => Inst.Inst -> BodyBuilderT m Val.Symbol
tellInst' inst = tellInst inst >>= \case
  Just s -> pure s
  Nothing -> error ("Expected instruction " ++ instName inst ++ " to return a symbol!")

-- | Adds a new block to the function.
tellNewBlock :: (HasCallStack, Monad m) => BodyBuilderT m BasicBlockId
tellNewBlock = do
  blockId <- getsCtx (fromIntegral . nBlocks)
  modifyCtx (\c -> c { nBlocks = (nBlocks c) + 1, blocks = (Fn.BasicBlock []):blocks c})
  return blockId

askBlocks :: (HasCallStack, Monad m) => BodyBuilderT m [Fn.BasicBlock]
askBlocks = reverseBlocks <$> getsCtx blocks
  where
    reverseBlocks :: [Fn.BasicBlock] -> [Fn.BasicBlock]
    reverseBlocks = map (Fn.bbmap reverse) . reverse

takeBlocks :: (HasCallStack, Monad m) => Int -> BodyBuilderT m [Fn.BasicBlock]
takeBlocks i = do
  blocks <- askBlocks
  modifyCtx (\ctx -> ctx { nInst = i
                         , nRef =0
                         , nBlocks = 0
                         , blocks = mempty
                         })
  return blocks


--

buildResolver :: (HasCallStack, Monad m) => BodyBuilderT m Resolver
buildResolver = do
  defFns <- askFuncs
  refFns <- askDecls
  globs  <- askGlobals
  labels <- Set.toList <$> askLabels
  when llvmDebugResolverFlag $
    forM_ (zip [0..] labels) $ \(i, l) ->
      Trace.traceM $ (show i) ++ ": " ++ show l
  -- Note [Duplicate names in labels]
  --      We may run into the following situation:
  -- during construction we add labels of the same
  -- name to the set of labels.  Some of these may
  -- carry a default and some not.  This usually
  -- happens when we have function symbols that are
  -- cast into ints.  We have one label of type Int
  -- and one with a function type for the same name.
  --
  --      To work around this, we will sort the
  -- labels in the following way. First take all
  -- labels that contain a default value.  And then
  -- take all other labes, that do not have a default
  -- value, and are not part int he first set.
  --

  let allValues      = Set.toList $ Set.unions [defFns, refFns, globs]
      namedValuesMap = Map.fromList [(n, s) | s@(Val.Named n _i _t _v) <- allValues]
      --
      (labelsWithDefs, labelsWithDefNames)
        = unzip [(l,n) | l@(Label (n, _, Just _)) <- labels]
      (otherLabels, otherLabelNames)
        = unzip [(l,n) | l@(Label (n, _, Nothing)) <- labels
                       , not (n `elem` labelsWithDefNames)]
      labels' = labelsWithDefs ++ otherLabels

  labelMap <- Map.fromListWith (error "Duplicate Key in Labels")
              <$> (forM labels' $ \(Label (n, t, mbVal)) -> case (Map.lookup n namedValuesMap, mbVal) of
                      (Just s,  _      ) -> return (n, s)
                      (Nothing, Just v ) -> (n,) <$> tellGlobal (Val.mkNamed t n v)
                      (Nothing, Nothing) -> (n,) <$> tellGlobal (Val.mkNamed t n (defGlobal t)))

  return $ \key -> fromMaybe (error "bad resolver!")
                   . flip Map.lookup labelMap
                   $ key

setResolver :: (HasCallStack, Monad m) => Resolver -> BodyBuilderT m ()
setResolver r = modifyCtx (\ctx -> ctx { resolver = r })

type Offset = Word64 -> Word64

setGOffset, setDOffset, setFOffset, setCOffset :: (HasCallStack, Monad m) => Offset -> BodyBuilderT m ()
setGOffset o = modifyCtx (\ctx -> ctx { gOffset = o })
setDOffset o = modifyCtx (\ctx -> ctx { dOffset = o })
setFOffset o = modifyCtx (\ctx -> ctx { fOffset = o })
setCOffset o = modifyCtx (\ctx -> ctx { cOffset = o })

