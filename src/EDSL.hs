{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase        #-}

module EDSL
  ( module EDSL.Types
  , module EDSL.Values
  , module EDSL.Instructions
  , global, extGlobal
  , ghcfun, fun
  , block, block', block''
  , def, defT
  , ghcdef, ghcdefT
  , mod
  , writeModule
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
import Data.BitCode.LLVM (Module(..), Ident(..))
import Data.BitCode.LLVM.Classes.HasType
import Data.BitCode.LLVM.Classes.ToSymbols (symbols)
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

-- | create a global constant
global :: String -> Val.Value -> Val.Symbol
global name val = Val.Named name $ defGlobal {Val.gPointerType = ptr (ty val), Val.gInit = Just (Val.Unnamed val) }

-- | create an external global constant
extGlobal :: String -> Ty.Ty -> Val.Symbol
extGlobal name ty = Val.Named name $ defGlobal { Val.gPointerType = ptr ty }

-- by default we assume every function is defined. We will turn them indo declarations
-- later if we do not find a matching function body.
fun :: String -> Ty.Ty -> Val.Symbol
fun name sig = Val.Named name $ defFunction { Val.fType = ptr sig, Val.fIsProto = False }

ghcfun :: String -> Ty.Ty -> Val.Symbol
ghcfun name sig = Val.Named name $ defFunction { Val.fType = ptr sig, Val.fIsProto = False, Val.fCallingConv = CallingConv.GHC }

-- | BasicBlocks
block'' :: Monad m => l -> BodyBuilderT m a -> BodyBuilderT m ((l, BasicBlockId), a)
block'' name instructions = do
  bbRef <- tellNewBlock
  ((name, bbRef),) <$> instructions

block' :: Monad m => l -> BodyBuilderT m a -> BodyBuilderT m (l, BasicBlockId)
block' name instructions = fst <$> block'' name instructions

block :: Monad m => l -> BodyBuilderT m a -> BodyBuilderT m BasicBlockId
block l = fmap snd . block' l

-- | Function definition
defT :: Monad m
     => String                             -- ^ The name of the function
     -> Ty.Ty                              -- ^ The function signature ([x] --> y)
     -> ([Val.Symbol] -> BodyBuilderT m a) -- ^ The body generator (symbols are references to the functions arguments)
     -> m Func.Function
defT name sig body = Func.Function (Val.Named name $ defFunction { Val.fType = ptr sig, Val.fIsProto = False}) [] <$> execBodyBuilderT 0 (body args)
  where args = map Val.Unnamed $ map Val.Arg (Ty.teParamTy sig)

def :: String -> Ty.Ty -> ([Val.Symbol] -> BodyBuilder a) -> Func.Function
def name sig = runIdentity . defT name sig

ghcdefT :: Monad m
        => String
        -> Ty.Ty
        -> ([Val.Symbol] -> BodyBuilderT m a)
        -> m Func.Function
ghcdefT name sig body = Func.Function (Val.Named name $ defFunction { Val.fType = ptr sig, Val.fIsProto = False, Val.fCallingConv = CallingConv.GHC }) [] <$> execBodyBuilderT 0 (body args)
  where args = map Val.Unnamed $ map Val.Arg (Ty.teParamTy sig)

ghcdef :: String -> Ty.Ty -> ([Val.Symbol] -> BodyBuilder a) -> Func.Function
ghcdef name sig = runIdentity . ghcdefT name sig

-- | Module
mod :: String -> [Func.Function] -> Module
mod name fns = Module 1 Nothing Nothing ((nub . sort $ concatMap (globalConstants . Val.symbolValue) globals) ++ (filter (not . isFunction) globals ++ decls ++ defFns)) decls (map (collectFnConstants . updateFnRefs) fns)
  where globals = filter isGlobal (concatMap symbols fns)
        -- defined functions
        defFns = map Func.dSig fns
        -- references functions
        refFns = filter isFunction globals
        -- decls, are all non defined functions.
        -- and they are set to be prototypes
        declMap = map (\f@(Val.Named n f') -> (f, Val.Named n f' { Val.fIsProto = True})) $
          foldl (\acc f -> if f `elem` defFns then acc
                           else if f `elem` acc then acc
                                else f:acc) [] refFns
        updateInst :: Inst.Inst -> Inst.Inst
        updateInst i@(Inst.Call { Inst.cSym = s }) | (Val.Function{}) <- Val.symbolValue s
                                                   , Just f'          <- lookup s declMap  = i { Inst.cSym = f' }
                                              | otherwise = i
        updateInst i = i

        -- Map the @updateInst@ fn over all function instructions
        -- to update the function references.

        updateFnRefs :: Func.Function -> Func.Function
        updateFnRefs = Func.fbmap (Func.bimap (Func.imap updateInst))
        decls = map snd declMap
        -- defined functions
        -- XXX: TODO? Globals can contain constants. So we need to extract
        --            them, as they do not (incorrectly?) follow from the
        --            symbols call. If they would however, it would be
        --            hard to distinguish between fn level constants and
        --            global constants.
        globalConstants (Val.Global{..}) | Just v <- gInit = [v]
                                         | otherwise       = []
        globalConstants _                                  = []
        -- We'll remove duplicate constants from function bodies.
        collectFnConstants f = f { Func.dConst = nub . sort $ filter isFnConstant (symbols f) }
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

--------------------------------------------------------------------------------
-- I/O
writeModule :: FilePath -> Module -> IO ()
writeModule f = writeFile f . withHeader True . emitTopLevel . map denormalize . toBitCode . (Just (Ident "Data.BitCode.LLVM" Current),)

