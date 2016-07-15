module Data.BitCode.LLVM.Attribute where

-- | PARAMATTR blocks have code for defining a parameter attribute set.
data AttributeCodes
  = PARAMATTR_UNUSED0 -- 0
  -- | ENTRY: [paramidx0, attr0, paramidx1, attr1...]. WARN: Will be removed in 4.0
  | PARAMATTR_CODE_ENTRY_OLD -- 1
  -- | ENTRY: [paramidx0, attrgrp0, paramidx1, attrgrp1, ...]
  | PARAMATTR_CODE_ENTRY -- 2
  -- | ENTRY: [id, attr0, att1, ...]
  | PARAMATTR_GRP_CODE_ENTRY -- 3
  deriving (Show, Enum)
