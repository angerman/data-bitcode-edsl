module Data.BitCode.LLVM where

import Data.BitCode.LLVM.Module

--- LLVM Bit Codes -------------------------------------------------------------
-- see LLVMBitCodes.h (e.g. http://llvm.org/docs/doxygen/html/LLVMBitCodes_8h_source.html)
--
-- Emacs Query Replace:
--  regexp: ^[[:digit:]]+[[:space:]]+\([[:alpha:]_]+\)[[:space:]]*=[[:space:]]*\([[:digit:]]+\)[,[:space:]]*//[[:space:]]*\(.+\)
--  with: -- | \3^J  | \1 -- \2
--  ^J: C-q C-j
--
--  if you did not copy off the website, you won't need the [[:digit:]]+ in front.
--  some fixing by hand is still required.  As some comments span multiple lines.
--

-- In general, the BlockIDs can be found in LLVM.IDs
-- the coresponding Record Codes in LLVM.Codes.XYZ

data Block
  = Module { {- InfoBlock -} mRecords :: [Module]
           , mBlocks :: [Block] }
  | ParamAttr
  | ParamAttrGrp
  | Constants
  | Function
  | Identification
  | ValueSymTab
  | Metadata
  | MetadataAttachment
  | Type
  | UseList
  | ModuleStartAB
  | FunctionSummary
  | OperandBundleTags
  | MetadataKind
  deriving Show
