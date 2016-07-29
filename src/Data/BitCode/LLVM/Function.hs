module Data.BitCode.LLVM.Function where

import Data.BitCode.LLVM.Value       (Symbol, Value)
import Data.BitCode.LLVM.Instruction (Inst)

-- | Function declarations are set of so called basic blocks,
-- which contain sets of instructions.  These blocks may have
-- labels.
type BlockInst = (Maybe Symbol, Inst)

data BasicBlock
  = BasicBlock [BlockInst]
  | NamedBlock Label [BlockInst]
  deriving Show

type Label = String

-- | Function definitions.
-- TODO: dSig is somewhat ugly, I'd lke enforce only function values here.
data Function = Function { dSig :: Symbol, dConst :: [Symbol], dBody :: [BasicBlock] }
  deriving Show