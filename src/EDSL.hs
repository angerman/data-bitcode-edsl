{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase        #-}

module EDSL where

import Prelude hiding (mod, writeFile)

import Data.BitCode.Writer.Monad (writeFile)
import Data.BitCode.Writer (emitTopLevel)
import Data.BitCode.Writer.Combinators (withHeader)
import Data.BitCode.LLVM.ToBitCode
import Data.BitCode (denormalize)
import Data.BitCode.LLVM.Codes.Identification (Epoch(Current))

import Text.PrettyPrint (Doc)
import Data.BitCode.LLVM.Pretty
import Data.BitCode.LLVM.Types
import Data.BitCode.LLVM (Module(..), Ident(..), ty)
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

--------------------------------------------------------------------------------
-- TODO:
-- - Fix monad (mapM_ does not work)
-- - sample mkModule
--   - collect globals
--   - turn into proper module
--   - write to bitcode
--------------------------------------------------------------------------------


-- reexport pretty
pp :: (Pretty a) => a -> Doc
pp = pretty

-- create a pointer with 0 addressSpace
ptr :: Ty.Ty -> Ty.Ty
ptr = Ty.Ptr 0

-- Some basic types
i8, i32, i8ptr, i32ptr :: Ty.Ty
i8 = Ty.Int 8
i32 = Ty.Int 32
i64 = Ty.Int 64
i8ptr = ptr i8
i32ptr = ptr i32
i64ptr = ptr i64
void = Ty.Void
arr n t = Ty.Array (fromIntegral n) t

-- type constructors for function types
(-->) :: [Ty.Ty] -> Ty.Ty -> Ty.Ty
ts --> t = Ty.Function False t ts
vararg f@(Ty.Function{}) = f { Ty.teVarArg = True }

-- values
v_null = Val.Null
v_int n = Val.Int n
v_wInt [n] = Val.WideInt n
v_str s = Val.String s
v_cStr s = Val.CString s

-- Some def'ault constructors
defLinkage = Linkage.External
defVisibility = Visibility.Default
defTLM = ThreadLocalMode.NotThreadLocal
defStorageClass = StorageClass.Default
defCC = CallingConv.C
defSymbol :: Val.Symbol
defSymbol = undefined

-- Globals (globals, functions, aliases)
defGlobal, defFunction, defAlias :: Val.Value
defGlobal   = Val.Global void True 0 Nothing defLinkage 0 0 defVisibility defTLM False False defStorageClass 0
defFunction = Val.Function void defCC True defLinkage 0 0 0 defVisibility 0 False 0 defStorageClass 0 0 0
defAlias    = Val.Alias void 0 defSymbol defLinkage defVisibility defTLM False defStorageClass

-- * Constant Values
cStr s = Val.Constant (arr (1 + length s) i8) (v_cStr s)
str s = Val.Constant (arr (length s) i8) (v_str s)

-- * Constant Symbols
int8  = Val.Unnamed . Val.Constant i8 . v_int
int32 = Val.Unnamed . Val.Constant i32 . v_int
int64 = Val.Unnamed . Val.Constant i64 . v_int

-- * Instructions
callI f args = Inst.Call (Ty.teRetTy . Ty.tePointeeTy $ ty f) f args
retI = Inst.Ret . Just
gepI s = Inst.Gep (Ty.tePointeeTy sTy) True s . map int32
  where sTy = ty s
-- * Symbols
-- | unnamed constant CString
-- cStr = Val.Unnamed . v_cStr

-- tbd fn
-- mkMod :: String -> _ -> Module
-- mkMod = undefined

-- | create a global constant
global :: String -> Val.Value -> Val.Symbol
global name val = Val.Named name $ defGlobal {Val.gPointerType = ptr (ty val), Val.gInit = Just val }

fun :: String -> Ty.Ty -> Val.Symbol
fun name sig = Val.Named name $ defFunction { Val.fType = ptr sig }

--------------------------------------------------------------------------------
-- Monadic interface (BodyBuilder)
-- | Intructions
gep s = tellInst' . gepI s
call f = tellInst' . callI f
ret  = tellInst . retI
-- | BasicBlocks
block :: String -> BodyBuilder a -> BodyBuilder BasicBlockId
block name instructions = do
  bbRef <- tellNewBlock
  instructions
  pure bbRef
-- | Function
def name sig = Func.Function (Val.Named name $ defFunction { Val.fType = ptr sig, Val.fIsProto = False}) [] . execBodyBuilder
-- | Module
mod :: String -> [Func.Function] -> Module
mod name fns = Module 1 Nothing Nothing ((concatMap (globalConstants . Val.symbolValue) globals) ++ globals) decls (map collectFnConstants fns)
  where globals = filter isGlobal (concatMap symbols fns)
        -- XXX: HACK BEGIN. Globals can contain constants. So we need to extract
        --                  them, as they do not (incorrectly?) follow from the
        --                  symbols call. If they would however, it would be
        --                  hard to distinguish between fn level constants and
        --                  global constants.
        globalConstants (Val.Global{..}) | Just v <- gInit = [(Val.Unnamed v)]
                                         | otherwise       = []
        globalConstants _                                  = []
        -- XXX: HACK END
        decls   = [f | f@(Val.Named _ (Val.Function{..})) <- globals, fIsProto ]
        collectFnConstants f = f { Func.dConst = filter isFnConstant (symbols f) }
        isGlobal x
          | (Val.Global{})   <- Val.symbolValue x = True
          | (Val.Function{}) <- Val.symbolValue x = True
          | (Val.Alias{})    <- Val.symbolValue x = True
          | otherwise                             = False
        isFnConstant x
          | (Val.Constant{}) <- Val.symbolValue x = True
          | otherwise                             = False


helloWorld = mod "helloWorld"
  [ def "main" ([i32, ptr i8ptr] --> i32) $ do
      block "entry" $ do
        strPtr <- gep (global "foo" (cStr "hello world\n")) [0, 0]
        call (fun "printf" (vararg $ [i8ptr] --> i32)) [strPtr]
        ret $ int32 0
  ]


--------------------------------------------------------------------------------
-- I/O
writeModule :: FilePath -> Module -> IO ()
writeModule f = writeFile f . withHeader True . emitTopLevel . map denormalize . toBitCode . (Just (Ident "Data.BitCode.LLVM" Current),)

--------------------------------------------------------------------------------
-- Function Body Monad... or building blocks is monadic.
-- We want to be able to obtain references for later use
-- and we need to provide some form of unique symbol supply
-- the simples being a counter.

data FnCtx = FnCtx { nInst :: Int, nBlocks :: Int, blocks :: [Func.BasicBlock] } deriving Show
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
bmap :: ([Func.BlockInst] -> [Func.BlockInst]) -> Func.BasicBlock -> Func.BasicBlock
bmap f (Func.BasicBlock bi) = (Func.BasicBlock (f bi))
bmap f (Func.NamedBlock n bi) = (Func.NamedBlock n (f bi))

execBodyBuilder :: BodyBuilder a -> [Func.BasicBlock]
execBodyBuilder = map (bmap reverse) . reverse . blocks . snd . flip runBodyBuilder (FnCtx 0 0 mempty)

instance Functor BodyBuilder where
  fmap f b = BodyBuilder $ \c -> let (x, s) = runBodyBuilder b c in (f x, s)

instance Applicative BodyBuilder where
  pure x = BodyBuilder $ \c -> (x, c)
  a <*> b = error $ "No applicative interface!"

instance Monad BodyBuilder where
  m >>= k = BodyBuilder $ \s -> let (x, s') = runBodyBuilder m s
                                in runBodyBuilder (k x) s'

addInst :: Func.BasicBlock -> Func.BlockInst -> Func.BasicBlock
addInst (Func.BasicBlock is) i = (Func.BasicBlock (i:is))
addInst (Func.NamedBlock n is) i = (Func.NamedBlock n (i:is))

tellInst :: Inst.Inst -> BodyBuilder (Maybe Val.Symbol)
tellInst inst = BodyBuilder $ \(FnCtx ni nb (b:bbs)) -> let ref = Val.Unnamed . flip Val.TRef ni <$> Inst.instTy inst
                                                        in (ref, FnCtx (ni + 1) nb ((addInst b (ref, inst)):bbs))
tellInst' :: Inst.Inst -> BodyBuilder Val.Symbol
tellInst' inst = tellInst inst >>= \case
  Just s -> pure s
  Nothing -> fail ("Expected instruction " ++ show inst ++ " to return a symbol!")

tellNewBlock :: BodyBuilder BasicBlockId
tellNewBlock = BodyBuilder $ \(FnCtx ni nb bbs) -> (fromIntegral nb+1, FnCtx ni (nb + 1) ((Func.BasicBlock []):bbs))
