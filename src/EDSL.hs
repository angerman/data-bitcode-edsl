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
  , def, defT
  , ghcdef, ghcdefT
  , mod, mod'
  , writeModule
  , dumpModuleBitcodeAST
  , withPrefixData
  , withPrefixDataM
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
     -> EdslT m Func.Function
defT name sig body = do sig' <- sig
                        body (args sig')
                        mkFunc sig'
  where args sig = map Val.Unnamed $ zipWith Val.Arg (Ty.teParamTy sig) [0..]
        mkFunc :: (HasCallStack, Monad m) => Ty.Ty -> EdslT m Func.Function
        mkFunc sig = Func.Function <$> deffun name sig <*> pure [] <*> lift (takeBlocks 0)
 
def :: HasCallStack => String -> Edsl Ty.Ty -> ([Val.Symbol] -> Edsl a) -> Edsl Func.Function
def name sig = defT name sig

ghcdefT :: (HasCallStack, Monad m)
        => String
        -> EdslT m Ty.Ty
        -> ([Val.Symbol] -> EdslT m a)
        -> EdslT m Func.Function
ghcdefT name sig body = do sig' <- sig
                           body (args sig')
                           mkFunc sig'
  where args sig = map Val.Unnamed $ zipWith Val.Arg (Ty.teParamTy sig) [0..]
        mkFunc :: (HasCallStack, Monad m) => Ty.Ty -> EdslT m Func.Function
        mkFunc sig = Func.Function <$> ghcfun name sig <*> pure [] <*> lift (takeBlocks 0) 

ghcdef :: HasCallStack => String -> Edsl Ty.Ty -> ([Val.Symbol] -> Edsl a) -> Edsl Func.Function
ghcdef name sig = ghcdefT name sig

-- | Prefix Data
withPrefixData :: HasCallStack => Val.Symbol -> Func.Function -> Func.Function
withPrefixData dat f = f { Func.dSig = sig' }
  where sig = Func.dSig f
        sig' = (\x -> x { Val.fExtra = (Val.fExtra x) { Val.fePrefixData = pure dat }}) <$> sig

-- | withPrefixData, if the prefixdata needs to run in the Builder.
withPrefixDataM :: (HasCallStack, Monad m) => EdslT m Val.Symbol -> EdslT m Func.Function -> EdslT m Func.Function
withPrefixDataM prefixGen f = withPrefixData <$> prefixGen <*> f

-- | Module
mod :: HasCallStack => String -> [Edsl Func.Function] -> Module
mod name = mod' name []

-- | mod' module creation. This does the heavy lifting.
mod' :: HasCallStack
     => String               -- ^ Module name
     -> [Edsl Val.Symbol]    -- ^ module globals
     -> [Edsl Func.Function] -- ^ module functions
     -> Module
mod' name modGlobals fns = let (resolver, m) = case evalEdsl resolver 0 go of
                                 Left e -> error e
                                 Right res -> res
                           in m 
  where go :: Edsl (String -> Val.Symbol, Module)
        go = do
          let r = error "no resolver!"
          _ <- sequence modGlobals
          fns' <- sequence fns
          globals <- Set.toList <$> lift askGlobals
          labels <- lift askLabels

          let defFns :: [Val.Symbol]
              defFns = map Func.dSig fns'
              refFns :: [Val.Symbol]
              refFns = [f | f <- globals, isFunction f, not (f `elem` defFns)]
              globalValues = sort $ filter (not . isFunction) globals
              allValues = defFns ++ refFns ++ globalValues
              allFnConstants = filter isFnConstant allValues 
              namedValuesMap :: [(String, Val.Symbol)]
              namedValuesMap = [(n, s) | s@(Val.Named n _) <- allValues]
              labelMap :: [(String, Val.Symbol)]
              extGlobals :: [Val.Symbol]
              (labelMap, extGlobals) = foldl f ([],[]) labels
                where f (lm, gs) (n, t) = case lookup n namedValuesMap of
                                            Just s  -> ((n,s):lm, gs)
                                            Nothing -> let g = extGlobal_ n t in ((n,g):lm, g:gs)
              globalValues' = extGlobals ++ globalValues
              defFnMap = zip defFns defFns
              refFnMap = zip refFns refFns
              fixFunction :: Val.Symbol -> Val.Symbol
              fixFunction s | (Val.Function{}) <- Val.symbolValue s = fromMaybe (error "Unable to locate function.") ((lookup s defFnMap) <|> (lookup s refFnMap))
              fixFunction x = x


              -- update a function
              -- 2. collect all function constants.
              --    a. collect all symbols (starting from the allValues), as we do not want them to be taken apart here.
              --    b. filter out allValues, as they are global and not function local.
              --    c. only take the constant symbols.
              updateFunction :: Func.Function -> Func.Function 
              updateFunction f
                = f { Func.dConst =  []
                      -- sort
                      -- . filter (\x -> not (x `elem` allFnConstants))
                      -- . filter isFnConstant
                      -- $ Set.toList cs
                      -- set the updated (with fixed lables) body.
                    , Func.dBody  = body
                    }
                where
                  body :: [Func.BasicBlock]
                  body = map (Func.bimap (Func.imap (updateInst fixFunction))) (Func.dBody f)
                             
                                     
          types <- lift askTypes
          consts <- lift askConsts
          return (r, Module
                   { mVersion    = 1
                   , mTriple     = Nothing
                   , mDatalayout = Nothing
                   , mValues = globalValues'
                   -- TODO: This is looks aweful. However we ended up with non prototype fn's here. :(
                   , mDecls = map (fmap mkDecl) refFns
                   , mFns = map updateFunction fns'
                   , mTypes = types 
                   , mConsts = consts 
                   })

--------------------------------------------------------------------------------
-- I/O
writeModule :: HasCallStack => FilePath -> Module -> IO ()
writeModule f = writeFile f . withHeader True . emitTopLevel . map denormalize . toBitCode . (Just (Ident "Data.BitCode.LLVM" Current),)

dumpModuleBitcodeAST :: HasCallStack => FilePath -> Module -> IO ()
dumpModuleBitcodeAST f = encodeFile f . map denormalize . toBitCode . (Just (Ident "Data.BitCode.LLVM" Current),)

--------------------------------------------------------------------------------
-- Predicates
isFunction, isGlobal, isFnConstant :: HasCallStack => Val.Symbol -> Bool
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

-- * Update Instructions.
updateInst :: HasCallStack => (Val.Symbol -> Val.Symbol) -> Inst.Inst -> Inst.Inst
updateInst fixFunction = \case
  i@(Inst.Call {..})         ->  i { Inst.cSym  = fixFunction cSym }
  x -> x
