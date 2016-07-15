module Data.BitCode.LLVM.Opcodes.Cast where

-- | CastOpcodes - These are values used in the bitcode files to encode which
-- cast a CST_CODE_CE_CAST or a XXX refers to.  The values of these enums
-- have no fixed relation to the LLVM IR enum values.  Changing these will
-- break compatibility with old files.
data Cast
  = CAST_TRUNC -- 0
  | CAST_ZEXT -- 1
  | CAST_SEXT -- 2
  | CAST_FPTOUI -- 3
  | CAST_FPTOSI -- 4
  | CAST_UITOFP -- 5
  | CAST_SITOFP -- 6
  | CAST_FPTRUNC -- 7
  | CAST_FPEXT -- 8
  | CAST_PTRTOINT -- 9
  | CAST_INTTOPTR -- 10
  | CAST_BITCAST -- 11
  | CAST_ADDRSPACECAST -- 12
  deriving (Show, Enum)
