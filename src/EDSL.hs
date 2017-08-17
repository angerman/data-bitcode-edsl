{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fprof-auto #-} 
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase        #-}

module EDSL
  ( module EDSL.Monad.Types
  , module EDSL.Monad.Values
  , module EDSL.Monad.Instructions
  , Result(..)
  , block, block', block''
  , def, defT, def_
  , ghcdef, ghcdefT
  , mod, mod'
  , writeModule
  , dumpModuleBitcodeAST
  , withPrefixData
  , withPrefixDataM
  , withPrefixDataM_
  )
where

import EDSL.Monad
import EDSL.Monad.Internal
import EDSL.Monad.Types
import EDSL.Monad.Values
import EDSL.Monad.Instructions

import Prelude hiding (mod, writeFile)

import Data.BitCode.Writer.Monad (writeFile)
import Data.BitCode.Writer (emitTopLevel)
import Data.BitCode.Writer.Combinators (withHeader)
import Data.BitCode.LLVM.ToBitCode
import Data.BitCode (denormalize)
import Data.BitCode.LLVM.Codes.Identification (Epoch(Current))

import Data.BitCode.LLVM.Types
import Data.BitCode.LLVM.Util (lower)
import Data.BitCode.LLVM (Module(..), Ident(..))
import Data.BitCode.LLVM.Pretty (pretty)
import Data.BitCode.LLVM.Classes.HasType
import Data.BitCode.LLVM.Classes.ToSymbols (symbols, fsymbols)
import qualified Data.BitCode.LLVM.Instruction     as Inst
import qualified Data.BitCode.LLVM.Function        as Func
import qualified Data.BitCode.LLVM.Type            as Ty
import qualified Data.BitCode.LLVM.Value           as Val
import qualified Data.BitCode.LLVM.Linkage         as Linkage
import qualified Data.BitCode.LLVM.Visibility      as Visibility
import qualified Data.BitCode.LLVM.ThreadLocalMode as ThreadLocalMode
import qualified Data.BitCode.LLVM.StorageClass    as StorageClass
import qualified Data.BitCode.LLVM.CallingConv     as CallingConv

import qualified Data.BitCode.LLVM.Opcodes.Binary  as BinOp

import Data.Functor.Identity (runIdentity)

import Debug.Trace

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort, nub)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)

import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)

import Data.Binary (encodeFile)

import GHC.Stack (HasCallStack)

data Result
  = FunctionResult
  { _f :: Func.Function
  , _fglobals :: Set Val.Symbol
  , _fconsts :: Set Val.Symbol
  , _flabels :: Set Val.Symbol
  , _ftypes :: Set Ty.Ty
  }
  | DataResult
  { _d :: Val.Symbol
  , _dglobals :: Set Val.Symbol
  , _dconsts :: Set Val.Symbol
  , _dlabels :: Set Val.Symbol
  , _dtypes :: Set Ty.Ty
  }
  deriving (Show)

instance Val.HasLinkage Result where
  getLinkage (FunctionResult f _ _ _ _) = Val.getLinkage f
  getLinkage (DataResult d _ _ _ _) = Val.getLinkage d
  setLinkage l f@(FunctionResult{}) = f { _f = Val.setLinkage l (_f f) }
  setLinkage l d@(DataResult{}) = d { _d = Val.setLinkage l (_d d) }

-- | BasicBlocks
block'' :: (HasCallStack, Monad m) => l -> EdslT m a -> EdslT m ((l, BasicBlockId), a)
block'' name instructions = do
  bbRef <- lift tellNewBlock
  ((name, bbRef),) <$> instructions

block' :: (HasCallStack, Monad m) => l -> EdslT m a -> EdslT m (l, BasicBlockId)
block' name instructions = fst <$> block'' name instructions

block :: (HasCallStack, Monad m) => l -> EdslT m a -> EdslT m BasicBlockId
block l = fmap snd . block' l

-- | Function definition
defT :: (HasCallStack, Monad m)
     => String                             -- ^ The name of the function
     -> EdslT m Ty.Ty                      -- ^ The function signature ([x] --> y)
     -> ([Val.Symbol] -> EdslT m a)        -- ^ The body generator (symbols are references to the functions arguments)
     -> ExceptT Error m Result
defT name sig body = ExceptT . evalBodyBuilderT 0 . runExceptT $ do sig' <- sig
                                                                    body (args sig')
                                                                    mkFunc sig'
  where args sig = map Val.Unnamed $ zipWith Val.Arg (Ty.teParamTy sig) [0..]
        mkFunc :: (HasCallStack, Monad m) => Ty.Ty -> EdslT m Result
        mkFunc sig = do
          f <- Func.Function <$> deffun name sig <*> pure [] <*> lift askBlocks
          FunctionResult f <$> lift askGlobals <*> lift askConsts <*> lift askLabels <*> lift askTypes
 
def :: HasCallStack => String -> Edsl Ty.Ty -> ([Val.Symbol] -> Edsl a) -> Either Error Result
def name sig = runIdentity . runExceptT .  defT name sig

def_ :: HasCallStack => String -> Edsl Ty.Ty -> ([Val.Symbol] -> Edsl a) -> Result
def_ name sig body = case def name sig body of
  Left e -> error e
  Right f -> f

ghcdefT :: (HasCallStack, Monad m)
        => String
        -> EdslT m Ty.Ty
        -> ([Val.Symbol] -> EdslT m a)
        -> ExceptT Error m Result
ghcdefT name sig body = ExceptT . evalBodyBuilderT 0 . runExceptT $ do sig' <- sig
                                                                       body (args sig')
                                                                       mkFunc sig'
  where args sig = map Val.Unnamed $ zipWith Val.Arg (Ty.teParamTy sig) [0..]
        mkFunc :: (HasCallStack, Monad m) => Ty.Ty -> EdslT m Result
        mkFunc sig = do
          f <- Func.Function <$> ghcfun name sig <*> pure [] <*> lift askBlocks
          FunctionResult f <$> lift askGlobals <*> lift askConsts <*> lift askLabels <*> lift askTypes

ghcdef :: HasCallStack => String -> Edsl Ty.Ty -> ([Val.Symbol] -> Edsl a) -> Either Error Result
ghcdef name sig = runIdentity . runExceptT . ghcdefT name sig

-- | Prefix Data
withPrefixData :: HasCallStack => Val.Symbol -> Result -> Result
withPrefixData dat (FunctionResult f gs cs ls ts) = FunctionResult (f { Func.dSig = sig' }) gs cs ls ts
  where sig = Func.dSig f
        sig' = (\x -> x { Val.fExtra = (Val.fExtra x) { Val.fePrefixData = pure dat }}) <$> sig

-- | withPrefixData, if the prefixdata needs to run in the Builder.
withPrefixDataM :: (HasCallStack, Monad m) => EdslT m Val.Symbol -> Result -> ExceptT Error m Result
withPrefixDataM prefixGen res = ExceptT $ runBodyBuilderT' (runExceptT prefixGen)
  where
    runBodyBuilderT' :: (Monad m, Functor f) => BodyBuilderT m (f Val.Symbol) -> m (f Result)
    runBodyBuilderT' = fmap (\(a, s) -> fmap (addPrefixData res s) a) . runBodyBuilderT 0
    addPrefixData :: Result -> BodyBuilderResult -> Val.Symbol -> Result
    addPrefixData res prefixResult prefixData
      = withPrefixData prefixData (res { _flabels  = (_flabels res)  `Set.union` (_labels prefixResult)
                                       , _fglobals = (_fglobals res) `Set.union` (_globals prefixResult)
                                       , _fconsts  = (_fconsts res)  `Set.union` (_consts prefixResult)
                                       , _ftypes   = (_ftypes res)   `Set.union` (_types prefixResult)
                                       })

withPrefixDataM_ :: (HasCallStack) => Edsl Val.Symbol -> Result -> Result
withPrefixDataM_ prefixGen res = case go prefixGen res of
                                   Left e -> error e
                                   Right f -> f
  where go :: HasCallStack => Edsl Val.Symbol -> Result -> Either Error Result
        go prefixGen = runIdentity . runExceptT . withPrefixDataM prefixGen

-- | Module
mod :: HasCallStack => String -> [Result] -> Module
mod name = mod' name []

-- | mod' module creation. This does the heavy lifting.
mod' :: HasCallStack
     => String           -- ^ Module name
     -> [Result]         -- ^ module globals
     -> [Result]         -- ^ module functions
     -> Module
mod' name modGlobals fns = Module
  { mVersion    = 1
  , mTriple     = Nothing
  , mDatalayout = Nothing
  , mValues = globalValues'
  -- TODO: This is looks aweful. However we ended up with non prototype fn's here. :(
  , mDecls = map (fmap mkDecl) refFns'
  , mFns = map updateFunction fns
  }
  where fns' = map _f fns
        data' = map _d modGlobals

        -- globals' = data' ++ filter isGlobal (fsymbols [] fns')
        -- [TODO]: We are missing all the functions here. They
        --         are not recorded int he globals. isGlobal though
        --         triggers for Function, Global and Alias!
        --
        -- TODO  : so far only EDSL.fun and EDSL.ghcfun ahve been manually
        --         turned into gellGlobal in the Gen.hs, this should happen
        --         in the edsl, and thus be automatically enforced.
        globals = sort . Set.toList $ (Set.fromList $ map _d modGlobals) `Set.union` (Set.unions $ map _dglobals modGlobals) `Set.union` (Set.unions $ map _fglobals fns)

        -- defined functions
        defFns :: [Val.Symbol]
        defFns = map Func.dSig fns'
        -- references functions, all those functions in the globals
        -- that are *not* defined functions.
        refFns :: [Val.Symbol]
        refFns = [f | f <- globals, isFunction f, not (f `elem` defFns)]

        -- All global values, that are not functions (Global, Alias)
        globalValues = sort $ filter (not . isFunction) globals

        -- All Values we have. Functions, References and global Values.
        allValues = defFns ++ refFns ++ globalValues

        allFnConstants = filter isFnConstant allValues

        -- concrete values are those that are not labels
        concreteValues = filter (not . isLabel) allValues

        -- map names -> symbols
        namedValuesMap = [(n, s) | s@(Val.Named n _) <- concreteValues]

        -- labels references in globals (global, fn, alias)
        -- topLevelLabels = filter isLabel . fsymbols [] $ allValues
        -- function body labels
--        labels' = filter isLabel . fsymbols topLevelLabels $ fns'
        -- labels = filter isLabel . fsymbols topLevelLabels $ fns

        -- [TODO] Turn _dlabels and _flabels into Sets. We won't need the
        --        nub then anymore.
        labels = sort . Set.toList $ (Set.unions $ map _dlabels modGlobals) `Set.union` (Set.unions $ map _flabels fns)

        -- compute the label map, as well as external globals for
        -- references symbols that are not foundin the namedValues.
        labelMap :: [(Val.Symbol, Val.Symbol)]
        extGlobals :: [Val.Symbol]
        (labelMap, extGlobals) = foldl f ([],[]) labels
          where f (lm, gs) l@(Val.Named n (Val.Label t)) = case lookup n namedValuesMap of
                  Just s  -> ((l,s):lm, gs)
                  -- note that @extGlobal_@ takes the type verbatim. As
                  -- opposed to @extGlobal@, which lifts it.
                  Nothing -> let g = extGlobal_ n t in ((l,g):lm, g:gs)

        -- now we have the labelMap, which maps a label to a symbol; however, the
        -- symbol may still contain labels.

        ------------------------------------------------------------------------
        -- label free label map; defFns, refFns, and globalValues.
        labelMap'     = let m = map (\(l, s) -> (l, replaceLabels m s)) labelMap
                        in m
        defFns'       = map (replaceLabels labelMap') defFns
        refFns'       = map (replaceLabels labelMap') refFns
        globalValues' = extGlobals ++ map (replaceLabels labelMap') globalValues

        ------------------------------------------------------------------------
        -- label free instructions
        -- decls, are all non defined functions.
        -- and they are set to be prototypes

        -- The fnMap is just mapping a function to a function. However, we can then use
        -- lookup to find that function. What we basically want to do, is replace every
        -- function occurance with either one out of the defFns' or refFns'.
        defFnMap = zip defFns' defFns'
        refFnMap = zip refFns' refFns'

        fixFunction :: Val.Symbol -> Val.Symbol
        fixFunction s | (Val.Function{}) <- Val.symbolValue s = fromMaybe (error "Unable to locate function.") ((lookup s defFnMap) <|> (lookup s refFnMap))
        fixFunction x = x


        -- update a function
        -- 0. replace all labels in the function signature.
        -- 1. replace all labels in the body
        -- 2. collect all function constants.
        --    a. collect all symbols (starting from the allValues), as we do not want them to be taken apart here.
        --    b. filter out allValues, as they are global and not function local.
        --    c. only take the constant symbols.
        updateFunction :: Result -> Func.Function 
        updateFunction (FunctionResult f gs cs ls ts)
          = f { -- replace lables in the function signature.
                Func.dSig   = replaceLabels labelMap' (Func.dSig f)
                -- build the constants list.
              , Func.dConst =   sort
                                . filter (\x -> not (x `elem` allFnConstants))
                                . filter isFnConstant
                                $ Set.toList cs
                -- set the updated (with fixed lables) body.
              , Func.dBody  = body
              }
          where
            body :: [Func.BasicBlock]
            body = map (Func.bimap (Func.imap (updateInst fixFunction labelMap'))) (Func.dBody f)

--------------------------------------------------------------------------------
-- I/O
writeModule :: HasCallStack => FilePath -> Module -> IO ()
writeModule f = writeFile f . withHeader True . emitTopLevel . map denormalize . toBitCode . (Just (Ident "Data.BitCode.LLVM" Current),)

dumpModuleBitcodeAST :: HasCallStack => FilePath -> Module -> IO ()
dumpModuleBitcodeAST f = encodeFile f . map denormalize . toBitCode . (Just (Ident "Data.BitCode.LLVM" Current),)

--------------------------------------------------------------------------------
-- Predicates
isFunction, isGlobal, isFnConstant, isLabel :: HasCallStack => Val.Symbol -> Bool
isFunction x
  | (Val.Function{}) <- Val.symbolValue x = True
  | otherwise                             = False
isGlobal x
  | (Val.Global{})   <- Val.symbolValue x = True
  | (Val.Function{}) <- Val.symbolValue x = True
  | (Val.Alias{})    <- Val.symbolValue x = True
  | otherwise                             = False
isFnConstant x
  | (Val.Constant{}) <- Val.symbolValue x = True
  | otherwise                             = False
isLabel x
  | (Val.Label{})    <- Val.symbolValue x = True
  | otherwise                             = False

--------------------------------------------------------------------------------
-- Rethinking labels.
--
-- Ideally, we had Either Label Symbol
-- Then we could turn some AST from Either Label Symbol to Symbol, and had
-- pruned all labels with safety from the type system.
--
-- `mod` obtains Func.Function + some global symbols.
--
-- Symbols are currently Named Values.
-- Labels just have a type; Named is Named a.
--
-- That yak will not be shaved now.
--------------------------------------------------------------------------------
-- Label replacement for symbols.
type LabelMap = [(Val.Symbol, Val.Symbol)]

replaceLabel :: HasCallStack => LabelMap -> Val.Symbol -> Val.Symbol
replaceLabel m l@(Val.Named _ (Val.Label _)) = case lookup l m of
  Nothing -> error $ "FATAL: Label " ++ show (pretty l) ++ " not in labelMap!"
  Just s  | (ty l) == (ty s) -> s
          -- if both are pointers this is probably going to work, as labels
          -- are cast to int right now anyway. TODO: This should be fixed.
          | Ty.isPtr (ty l) && Ty.isPtr (ty s) -> s
          -- if they are not pointers, this is very likely going to break.
          | otherwise -> traceShow ("!! WARN: Ignoring type missmatch " ++ show (pretty s) ++ " to " ++ show (pretty (ty l))) $ s

-- Label replacement for instructions.
replaceLabel' :: HasCallStack => LabelMap -> Val.Symbol -> Val.Symbol
 -- turn functions into labels to be replaced by their right value.
replaceLabel' m l@(Val.Named _ (Val.Label _)) = case lookup l m of
  Nothing -> error $ "FATAL: Label " ++ show (pretty l) ++ " not in labelMap!"
  Just s | (ty l) == (ty s) -> s
         -- if both are pointers this is probably going to work, as labels
         -- are cast to int right now anyway. TODO: This should be fixed.
         | Ty.isPtr (ty l) && Ty.isPtr (ty s) -> s
         | otherwise -> traceShow ("!! WARN: Ignoring type missmatch " ++ show (pretty s) ++ " to " ++ show (pretty (ty l))) $ s

-- * Symbols
replaceLabels :: HasCallStack => LabelMap -> Val.Symbol -> Val.Symbol
replaceLabels m s | isLabel s = replaceLabel m s
                  | otherwise = fmap (replaceLabelsV m) s

replaceLabels' :: HasCallStack => LabelMap -> Val.Symbol -> Val.Symbol
replaceLabels' m s | isLabel s = replaceLabel' m s
                   | otherwise = fmap (replaceLabelsV' m) s

-- * Values
replaceLabelsV :: HasCallStack => LabelMap -> Val.Value -> Val.Value
replaceLabelsV m = \case
  g@(Val.Global{..}) -> g { Val.gInit = replaceLabels m <$> gInit }
  f@(Val.Function{..}) -> f { Val.fExtra = (Val.fExtra f) { Val.fePrologueData = replaceLabels m <$> Val.fePrologueData fExtra
                                                          , Val.fePrefixData   = replaceLabels m <$> Val.fePrefixData fExtra
                                                          }
                            }
  a@(Val.Alias{..}) -> a { Val.aVal = replaceLabels m aVal }
  c@(Val.Constant{..}) -> c { Val.cConst = replaceLabelsC m cConst }
  x -> x

replaceLabelsV' :: HasCallStack => LabelMap -> Val.Value -> Val.Value
replaceLabelsV' m = \case
  g@(Val.Global{..}) -> g { Val.gInit = replaceLabels m <$> gInit }
  f@(Val.Function{..}) -> f { Val.fExtra = (Val.fExtra f) { Val.fePrologueData = replaceLabels m <$> Val.fePrologueData fExtra
                                                          , Val.fePrefixData   = replaceLabels m <$> Val.fePrefixData fExtra
                                                          }
                            }
  a@(Val.Alias{..}) -> a { Val.aVal = replaceLabels m aVal }
  c@(Val.Constant{..}) -> c { Val.cConst = replaceLabelsC m cConst }
  x -> x

-- * Constants
replaceLabelsC :: HasCallStack => LabelMap -> Val.Const -> Val.Const
replaceLabelsC m = \case
  (Val.Array ss) -> Val.Array (map (replaceLabels m) ss)
  (Val.Vector ss) -> Val.Vector (map (replaceLabels m) ss)
  (Val.Struct ss) -> Val.Struct (map (replaceLabels m) ss)
  (Val.BinOp o s1 s2) -> Val.BinOp o (replaceLabels m s1) (replaceLabels m s2)
  (Val.Cast t o s) -> Val.Cast t o (replaceLabels m s)
  (Val.InboundsGep t ss) -> Val.InboundsGep t (map (replaceLabels m) ss)
  -- FIXME: Once the other @Const@ also have Symbols, they need to be added here!
  x -> x

-- * Instructions
replaceLabelI :: HasCallStack => LabelMap -> Val.Symbol -> Val.Symbol
-- turn functions into labels to be replaced by their right value.
replaceLabelI m l@(Val.Named _ (Val.Label _)) = case lookup l m of
  Nothing -> error $ "FATAL: Label " ++ show (pretty l) ++ " not in labelMap!"
  Just s | (ty l) == (ty s) -> s
         | otherwise -> traceShow ("! WARN: Ignoring type missmatch " ++ show (pretty s) ++ " to " ++ show (pretty (ty l))) $ s
replaceLabelI _ x = x

-- * Update Instructions, and replace labels with their values.
updateInst :: HasCallStack => (Val.Symbol -> Val.Symbol) -> LabelMap -> Inst.Inst -> Inst.Inst
updateInst fixFunction m = \case
  i@(Inst.Call {..})         ->  i { Inst.cSym  = fixFunction (replaceLabels m cSym)
                                   , Inst.cArgs = map (replaceLabels m) cArgs
                                   }
  (Inst.BinOp t o s1 s2 f)   -> Inst.BinOp t o (replaceLabelI m s1) (replaceLabelI m s2) f
  (Inst.Cast t o s)          -> Inst.Cast t o (replaceLabelI m s)
  (Inst.Load t s a)          -> Inst.Load t (replaceLabelI m s) a
  (Inst.Store s1 s2 a)       -> Inst.Store (replaceLabelI m s1) (replaceLabelI m s2) a
  (Inst.Cmp2 t s1 s2 p)      -> Inst.Cmp2 t (replaceLabelI m s1) (replaceLabelI m s2) p
  (Inst.Gep t b s ss)        -> Inst.Gep t b (replaceLabelI m s) (map (replaceLabelI m) ss)
  (Inst.Ret (Just s))        -> Inst.Ret (Just (replaceLabelI m s))
  (Inst.Br s l r)            -> Inst.Br (replaceLabelI m s) l r
  (Inst.Switch s d m')        -> Inst.Switch (replaceLabelI m s) d (map (\(s,b) -> (replaceLabelI m s, b)) m')
  x -> x
