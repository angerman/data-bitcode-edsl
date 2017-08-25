{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fprof-auto #-} 
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecursiveDo       #-}

module EDSL
  ( module EDSL.Monad.Types
  , module EDSL.Monad.Values
  , module EDSL.Monad.Instructions
  , Result(..)
  , block, block', block''
  , def, defT, defM
  , ghcdef, ghcdefT
  , mod, mod'
  , writeModule
  , dumpModuleBitcodeAST
  , withPrefixData
  )
where

import EDSL.Monad
import EDSL.Monad.Internal
import EDSL.Monad.Types
import EDSL.Monad.Values
import EDSL.Monad.Instructions
import EDSL.Monad.EdslT (evalEdslT, withResolver)

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

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (sort, nub, sortBy)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Control.Monad (liftM2)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)

import Data.Binary (encodeFile)

import GHC.Stack (HasCallStack)

import Debug.Trace
import Data.Function (on)

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
     => EdslT m (Val.Value -> Val.Value)   -- ^ Modifier
     -> String                             -- ^ The name of the function
     -> EdslT m Ty.Ty                      -- ^ The function signature ([x] --> y)
     -> ([Val.Symbol] -> EdslT m a)        -- ^ The body generator (symbols are references to the functions arguments)
     -> EdslT m Func.Function
defT mod name sig body = do sig' <- sig
                            body (args sig')
                            mkFunc sig' =<< mod
  where args sig = zipWith (\t -> Val.mkUnnamed t . Val.Arg t) (Ty.teParamTy sig) [0..]
        mkFunc :: (HasCallStack, Monad m) => Ty.Ty -> (Val.Value -> Val.Value) -> EdslT m Func.Function
        mkFunc sig mod = Func.Function <$> deffun mod name sig <*> pure [] <*> lift (takeBlocks 0)


defM :: HasCallStack
  => Edsl (Val.Value -> Val.Value)
  -> String
  -> Edsl Ty.Ty
  -> ([Val.Symbol] -> Edsl a)
  -> Edsl Func.Function
defM mod name sig = defT mod name sig

def :: HasCallStack => String -> Edsl Ty.Ty -> ([Val.Symbol] -> Edsl a) -> Edsl Func.Function
def name sig = defT (pure id) name sig

ghcdefT :: (HasCallStack, Monad m)
        => EdslT m (Val.Value -> Val.Value) -- ^ function modifier
        -> String
        -> EdslT m Ty.Ty
        -> ([Val.Symbol] -> EdslT m a)
        -> EdslT m Func.Function
ghcdefT mod name sig body = defT (liftM2 (.) mod (pure $ withCC CallingConv.GHC)) name sig body

-- ghcdefM :: (HasCalLStack

ghcdef :: HasCallStack => String -> Edsl Ty.Ty -> ([Val.Symbol] -> Edsl a) -> Edsl Func.Function
ghcdef name sig = ghcdefT (pure id) name sig

-- | Prefix Data
withPrefixData :: HasCallStack => Val.Symbol -> Val.Value -> Val.Value
withPrefixData dat f = f { Val.fExtra = (Val.fExtra f) { Val.fePrefixData = pure dat } }

-- | withPrefixData, if the prefixdata needs to run in the Builder.
-- withPrefixDataM :: (HasCallStack, Monad m) => EdslT m Val.Symbol -> EdslT m Func.Function -> EdslT m Func.Function
-- withPrefixDataM prefixGen f = withPrefixData <$> prefixGen <*> f

-- | Module
mod :: HasCallStack => String -> [Edsl Func.Function] -> Module
mod name fns = case runIdentity (mod' name [] fns) of
  Left e -> error e
  Right m -> m

-- | mod' module creation. This does the heavy lifting.
mod' :: (HasCallStack, Monad m, MonadFix m)
     => String               -- ^ Module name
     -> [EdslT m Val.Symbol]    -- ^ module globals
     -> [EdslT m Func.Function] -- ^ module functions
     -> m (Either Error Module)
mod' name modGlobals fns = evalEdslT 0 go 
  where go = mdo
          lift (setGOffset id)
          lift (setDOffset (\x -> fromIntegral nG + x))
          lift (setFOffset (\x -> fromIntegral (nG + nD) + x))
          lift (setCOffset (\x -> fromIntegral (nG + nD + nF) + x))
          
          fns' <- withResolver $ do
            fs <- sequence fns
            _ <- sequence modGlobals
            return fs

          nG <- Set.size <$> lift askGlobals
          nF <- Set.size <$> lift askFuncs
          nD <- Set.size <$> lift askDecls
          
          globalValues <-lift askGlobals
--          labels <- lift askLabels

          defFns <- lift askFuncs
          refFns <- lift askDecls
          let -- allValues = defFns ++ refFns ++ globalValues
              -- allFnConstants = filter isFnConstant allValues 

--              defFnMap = zip defFns defFns
              -- refFnMap = zip refFns refFns
              fixFunction :: Val.Symbol -> Val.Symbol
--              fixFunction s | (Val.Function{}) <- Val.symbolValue s = fromMaybe (error "Unable to locate function.") ((lookup s defFnMap) <|> (lookup s refFnMap))
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
              fixType :: Val.Symbol -> Val.Symbol
              fixType (Val.Named n i t v) = (Val.Named n i (ty v) v)
              fixType (Val.Unnamed i t v) = (Val.Unnamed i (ty v) v) 
              
          -- let labelMap' = Map.fromList labelMap
              -- resolver name = fromMaybe (error $ "unable to resovle function " ++ show name) $ Map.lookup name labelMap'
          types <- lift askTypes
          consts <- lift askConsts


          return Module
                   { mVersion    = 1
                   , mTriple     = Nothing
                   , mDatalayout = Nothing
                   , mValues = sortBy (compare `on` Val.symbolIndexValue) . Set.toList . Set.map fixType $ globalValues
                   , mDecls = sortBy (compare `on` Val.symbolIndexValue) . Set.toList $ refFns
                   , mDefns = sortBy (compare `on` Val.symbolIndexValue) . Set.toList $ defFns
                   , mFns = fns' -- map updateFunction fns'
                   , mTypes = types 
                   , mConsts = sortBy (compare `on` Val.symbolIndexValue) . Set.toList . Set.map fixType $ consts 
                   }

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
