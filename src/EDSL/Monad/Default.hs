module EDSL.Monad.Default where

import Data.BitCode.LLVM.Type (Ty, isPtr)
import Data.BitCode.LLVM.Value
import Data.BitCode.LLVM.CallingConv (CallingConv)

import qualified Data.BitCode.LLVM.CallingConv     as CallingConv
import qualified Data.BitCode.LLVM.Linkage         as Linkage
import qualified Data.BitCode.LLVM.Visibility      as Visibility
import qualified Data.BitCode.LLVM.ThreadLocalMode as ThreadLocalMode
import qualified Data.BitCode.LLVM.StorageClass    as StorageClass

-- | Defaults
defLinkage = Linkage.External
defVisibility = Visibility.Default
defTLM = ThreadLocalMode.NotThreadLocal
defStorageClass = StorageClass.Default
defCC = CallingConv.C
defSymbol :: Symbol
defSymbol = undefined

defGlobal, defFunction, defAlias :: Ty -> Value
defGlobal   ty = Global ty True 0 Nothing defLinkage 0 0 defVisibility defTLM False False defStorageClass 0
defFunction ty = Function ty defCC defLinkage 0 0 0 defVisibility 0 False defStorageClass 0 0 (FE True Nothing Nothing)
defAlias    ty = Alias ty 0 defSymbol defLinkage defVisibility defTLM False defStorageClass
