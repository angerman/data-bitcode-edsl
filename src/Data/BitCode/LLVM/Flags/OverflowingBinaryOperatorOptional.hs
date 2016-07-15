module Data.BitCode.LLVM.Flags.OverflowingBinaryOperatorOptional where

-- | OverflowingBinaryOperatorOptionalFlags - Flags for serializing
-- OverflowingBinaryOperator's SubclassOptionalData contents.
data OverflowingBinaryOperatorOptional
  = OBO_NO_UNSIGNED_WRAP -- 0
  | OBO_NO_SIGNED_WRAP -- 1
  deriving (Show, Enum)
