{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase        #-}

module EDSL
  ( module EDSL.Types
  , module EDSL.Values
  , module EDSL.Instructions
  , global, extGlobal, privateGlobal, internalGlobal
  , ghcfun, fun
  , block, block', block''
  , def, defT, def_
  , ghcdef, ghcdefT
  , mod, mod'
  , writeModule
  , dumpModuleBitcodeAST
  , withPrefixData
  , label
  )
where

import EDSL.Monad
import EDSL.Types
import EDSL.Values
import EDSL.Instructions

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

import Data.List (sort, nub)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)

import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)

import Data.Binary (encodeFile)

import GHC.Stack (HasCallStack)

-- | create a typed label to be resolved later.
label :: HasCallStack => String -> Ty.Ty -> Val.Symbol
label name = Val.Named name . Val.Label

-- | create a global constant
global :: HasCallStack => String -> Val.Value -> Val.Symbol
global name val = Val.Named name $ defGlobal { Val.gPointerType = ptr (ty val)
                                             , Val.gInit = Just (Val.Unnamed val) }

privateGlobal :: HasCallStack => String -> Val.Value -> Val.Symbol
privateGlobal name val = Val.Named name $ defGlobal  { Val.gPointerType = ptr (ty val)
                                                     , Val.gInit = Just (Val.Unnamed val)
                                                     , Val.gLinkage = Linkage.Private }
internalGlobal :: HasCallStack => String -> Val.Value -> Val.Symbol
internalGlobal name val = Val.Named name $ defGlobal { Val.gPointerType = ptr (ty val)
                                                     , Val.gInit = Just (Val.Unnamed val)
                                                     , Val.gLinkage = Linkage.Internal}

-- | create an external global constant
extGlobal :: HasCallStack => String -> Ty.Ty -> Val.Symbol
extGlobal name ty = Val.Named name $ defGlobal { Val.gPointerType = ptr ty }

fun :: HasCallStack => String -> Ty.Ty -> Val.Symbol
fun name sig = Val.Named name $ defFunction { Val.fType = ptr sig, Val.fIsProto = True }

ghcfun :: HasCallStack => String -> Ty.Ty -> Val.Symbol
ghcfun name sig = Val.Named name $ defFunction { Val.fType = ptr sig, Val.fIsProto = False, Val.fCallingConv = CallingConv.GHC }

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
     -> Ty.Ty                              -- ^ The function signature ([x] --> y)
     -> ([Val.Symbol] -> EdslT m a)        -- ^ The body generator (symbols are references to the functions arguments)
     -> ExceptT Error m Func.Function
defT name sig body = ExceptT $ runBodyBuilderT' 0 $ runExceptT (body args)
  where args = map Val.Unnamed $ zipWith Val.Arg (Ty.teParamTy sig) [0..]
        mkFunc :: [Func.BasicBlock] -> Func.Function
        mkFunc = Func.Function (Val.Named name $ defFunction { Val.fType = ptr sig, Val.fIsProto = False}) []
        runBodyBuilderT' :: (Monad m, Functor f) => Int -> BodyBuilderT m (f a) -> m (f Func.Function)
        runBodyBuilderT' i = fmap (\(a, s) -> fmap (const (mkFunc s)) a) . runBodyBuilderT i

def :: HasCallStack => String -> Ty.Ty -> ([Val.Symbol] -> Edsl a) -> Either Error Func.Function
def name sig = runIdentity . runExceptT .  defT name sig

def_ :: HasCallStack => String -> Ty.Ty -> ([Val.Symbol] -> Edsl a) -> Func.Function
def_ name sig body = case def name sig body of
  Left e -> error e
  Right f -> f

ghcdefT :: (HasCallStack, Monad m)
        => String
        -> Ty.Ty
        -> ([Val.Symbol] -> EdslT m a)
        -> ExceptT Error m Func.Function
ghcdefT name sig body = ExceptT $ runBodyBuilderT' 0 $ runExceptT (body args)
  where args = map Val.Unnamed $ zipWith Val.Arg (Ty.teParamTy sig) [0..]
        mkFunc :: [Func.BasicBlock] -> Func.Function
        mkFunc = Func.Function (Val.Named name $ defFunction { Val.fType = ptr sig, Val.fIsProto = False, Val.fCallingConv = CallingConv.GHC }) []
        runBodyBuilderT' :: (Monad m, Functor f) => Int -> BodyBuilderT m (f a) -> m (f Func.Function)
        runBodyBuilderT' i = fmap (\(a, s) -> fmap (const (mkFunc s)) a) . runBodyBuilderT i

ghcdef :: HasCallStack => String -> Ty.Ty -> ([Val.Symbol] -> Edsl a) -> Either Error Func.Function
ghcdef name sig = runIdentity . runExceptT . ghcdefT name sig

-- | Prefix Data
withPrefixData :: HasCallStack => Val.Symbol -> Func.Function -> Func.Function
withPrefixData dat f = f { Func.dSig = sig' }
  where sig = Func.dSig f
        sig' = (\x -> x { Val.fPrefixData = pure dat }) <$> sig

-- | Module
mod :: HasCallStack => String -> [Func.Function] -> Module
mod name = mod' name []
mod' :: HasCallStack => String -> [Val.Symbol] -> [Func.Function] -> Module
mod' name modGlobals fns = Module
  { mVersion    = 1
  , mTriple     = Nothing
  , mDatalayout = Nothing
  , mValues = globalValues'
  -- TODO: This is looks aweful. However we ended up with non prototype fn's here. :(
  , mDecls = map (fmap (\f -> f { Val.fIsProto = True })) refFns'
  , mFns = map updateFunction fns
  }
  where globals = modGlobals ++ filter isGlobal (fsymbols [] fns)

        -- defined functions
        defFns :: [Val.Symbol]
        defFns = map Func.dSig fns
        -- references functions, all those functions in the globals
        -- that are *not* defined functions.
        refFns :: [Val.Symbol]
        refFns = [f | f <- globals, isFunction f, not (f `elem` defFns)]

        -- All global values, that are not functions (Global, Alias)
        globalValues = sort $ filter (not . isFunction) globals

        -- All Values we have. Functions, References and global Values.
        allValues = defFns ++ refFns ++ globalValues

        -- concrete values are those that are not labels
        concreteValues = filter (not . isLabel) allValues

        -- map names -> symbols
        namedValuesMap = [(n, s) | s@(Val.Named n _) <- concreteValues]

        -- labels references in globals (global, fn, alias)
        topLevelLabels = filter isLabel . fsymbols [] $ allValues
        -- function body labels
        labels = filter isLabel . fsymbols topLevelLabels $ fns
        -- compute the label map, as well as external globals for
        -- references symbols that are not foundin the namedValues.
        labelMap :: [(Val.Symbol, Val.Symbol)]
        extGlobals :: [Val.Symbol]
        (labelMap, extGlobals) = foldl f ([],[]) labels
          where f (lm, gs) l@(Val.Named n (Val.Label t)) = case lookup n namedValuesMap of
                  Just s  -> ((l,s):lm, gs)
                  -- we lower the type, as we lifted it during label generation,
                  -- and externls are going to be of lifted type anyway.  Thus
                  -- without lowering it would end up being lifted twice.
                  Nothing -> let g = extGlobal n (lower t) in ((l,g):lm, g:gs)

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
        updateFunction f = f { -- replace lables in the function signature.
                               Func.dSig   = replaceLabels labelMap' (Func.dSig f)
                               -- build the constants list.
                             , Func.dConst = sort $ filter isFnConstant $ filter (\x -> not (x `elem` allValues)) (fsymbols allValues body)
                               -- set the fixed body.
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
  f@(Val.Function{..}) -> f { Val.fPrologueData = replaceLabels m <$> fPrologueData
                            , Val.fPrefixData   = replaceLabels m <$> fPrefixData
                            }
  a@(Val.Alias{..}) -> a { Val.aVal = replaceLabels m aVal }
  c@(Val.Constant{..}) -> c { Val.cConst = replaceLabelsC m cConst }
  x -> x

replaceLabelsV' :: HasCallStack => LabelMap -> Val.Value -> Val.Value
replaceLabelsV' m = \case
  g@(Val.Global{..}) -> g { Val.gInit = replaceLabels m <$> gInit }
  f@(Val.Function{..}) -> f { Val.fPrologueData = replaceLabels m <$> fPrologueData
                            , Val.fPrefixData   = replaceLabels m <$> fPrefixData
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
