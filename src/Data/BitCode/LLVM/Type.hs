module Data.BitCode.LLVM.Type where

import Data.Word (Word64)

-- * Types
data Ty
  = NumEntry Word64
  | Void | Float | Double | Label | Opaque
  | Int { teWidth :: Word64 }
  | Ptr { teAddressSpace :: Word64, tePointeeTy :: Ty }
--  | FnOld
  | Half
  | Array { teNumElts :: Word64, teEltTy :: Ty }
  | Vector { teNumElts :: Word64, teEltTy :: Ty }
  | X86Fp80
  | Fp128
  | Metadata
  | X86Mmx
  | StructAnon { teAnonIsPacked :: Bool, teAnonEltTy :: [Ty] }
  | StructName { teName :: String }
  | StructNamed { teNamedIsPacked :: Bool, teNamedEltTy :: [Ty] }
  | Function { teVarArg :: Bool, teRetTy :: Ty, teParamTy :: [Ty] }
  | Token
  deriving (Show, Eq, Ord)

subTypes :: Ty -> [Ty]
subTypes (Ptr _ t) = t:subTypes t
subTypes (Array _ t) = t:subTypes t
subTypes (Vector _ t) = t:subTypes t
subTypes (StructAnon _ ts) = concatMap (\t -> t:subTypes t) ts
subTypes (StructNamed _ ts) = concatMap (\t -> t:subTypes t) ts
subTypes (Function _ r pt) = concatMap (\t -> t:subTypes t) (r:pt)
subTypes _ = []
