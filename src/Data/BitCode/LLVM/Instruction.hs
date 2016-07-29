module Data.BitCode.LLVM.Instruction where

import Data.Word               (Word64)

import Data.BitCode.LLVM.Type  (Ty(Ptr))
import Data.BitCode.LLVM.Value (Symbol)
import Data.BitCode.LLVM.Cmp   (Predicate)
type Align = Word64


data Inst
  -- | Ty and Value type should match up. If Ty is Metadata, then the Value is takes from the Metadata List
  -- else from the value list.
  = Alloca Ty Symbol Align -- this would produce a typed ref. Ref
  -- | Load instruction
  | Load Ty Symbol Align
  -- | Store instruction. Store the Value in the Typed Ref.
  | Store Symbol Symbol Align
  -- | Call instruction. (Ty :: Ty, Fn :: Value, args :: [Value])
  | Call Ty Symbol [Symbol]
  -- | Compare
  | Cmp2 Ty Symbol Symbol Predicate
  -- | GEP
  | Gep
    Ty    -- ^ base type
    Bool  -- ^ inbounds
    Symbol -- ^ Value indexed into
    [Symbol] -- ^ indices.
  -- | Return Terminator
  | Ret (Maybe Symbol)
  -- | Unconditional branch
  | UBr Word64
  -- | Conditional branch
  | Br Symbol Word64 Word64
  deriving Show

instTy :: Inst -> Maybe Ty
instTy (Alloca t _ _) = Just t
instTy (Load t _ _) = Just t
instTy (Store{}) = Nothing
instTy (Call t _ _) = Just t
instTy (Ret{}) = Nothing
instTy (UBr{}) = Nothing
instTy (Br{}) = Nothing
instTy (Cmp2 t _ _ _) = Just t
-- GEP returns a pointer to it's type. In the
--     same address space.
-- TODO: This *is* incorrect.
--       the actual type woud have to be the type
--       of the index traversal into the value.
instTy (Gep t@(Ptr s _) _ _ _) = Just (Ptr s t)

isTerminator :: Inst -> Bool
isTerminator (Ret{}) = True
isTerminator (UBr{}) = True
isTerminator (Br{})  = True
isTerminator _       = False
