module Data.BitCode.LLVM.IDs.Blocks where
-- | The only top-level block type defined is for a module.
data BlockIDs
  -- | Skip the first 8 blocks. To make the offset at FIRST_APPLICATION_BLOCKID (8).
  = SKIP_0 | SKIP_1 | SKIP_2 | SKIP_3 | SKIP_4 | SKIP_5 | SKIP_6 | SKIP_7
  -- | Blocks
  | MODULE -- FIRST_APPLICATION_BLOCKID (8)
  -- | Module sub-block id's.
  | PARAMATTR -- 9
  | PARAMATTR_GROUP -- 10
  | CONSTANTS -- 11
  | FUNCTION -- 12
  -- | Block intended to contains information on the bitcode versioning.
  -- Can be used to provide better error messages when we fail to parse a
  -- bitcode file.
  | IDENTIFICATION -- 13
  | VALUE_SYMTAB -- 14
  | METADATA -- 15
  | METADATA_ATTACHMENT_ID -- 16
  | TYPE_NEW -- 17
  | USELIST -- 18
  | MODULE_STRTAB -- 19
  | FUNCTION_SUMMARY -- 20
  | OPERAND_BUNDLE_TAGS -- 21
  | METADATA_KIND -- 22
  deriving (Show, Enum)
