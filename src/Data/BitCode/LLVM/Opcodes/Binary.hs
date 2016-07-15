module Data.BitCode.LLVM.Opcodes.Binary where

-- | BinaryOpcodes - These are values used in the bitcode files to encode which
-- binop a CST_CODE_CE_BINOP or a XXX refers to.  The values of these enums
-- have no fixed relation to the LLVM IR enum values.  Changing these will
-- break compatibility with old files.
data Binary
  = BINOP_ADD -- 0
  | BINOP_SUB -- 1
  | BINOP_MUL -- 2
  | BINOP_UDIV -- 3
  -- | overloaded for FP
  | BINOP_SDIV -- 4
  | BINOP_UREM -- 5
  -- | overloaded for FP
  | BINOP_SREM -- 6
  | BINOP_SHL -- 7
  | BINOP_LSHR -- 8
  | BINOP_ASHR -- 9
  | BINOP_AND -- 10
  | BINOP_OR -- 11
  | BINOP_XOR -- 12
  deriving (Show, Enum)
