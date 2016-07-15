module Data.BitCode.LLVM.Module where

import Data.Word (Word64)

import Data.BitCode.LLVM.Linkage         (Linkage)
import Data.BitCode.LLVM.Visibility      (Visibility)
import Data.BitCode.LLVM.ThreadLocalMode (ThreadLocalMode)
import Data.BitCode.LLVM.StorageClass    (DLLStorageClass)
import Data.BitCode.LLVM.CallingConv     (CallingConv)

data Module
  = Version Word64
  | Triple String
  | Datalayout String
  | Asm String
  | Sectionname String
  -- | will be deprecated
  | Deplib String
  | GlobalVar
    { gvPointerType :: Word64          -- ^ The type index of the pointer type used to point to this global variable
    , gvIsConst :: Bool                -- ^ Non-zero if the variable is treated as constant within the module, or zero if it is not
    , gvInitId :: Int                  -- ^ If non-zero, the value index of the initializer for this variable, plus 1.
    , gvLinkage :: Linkage
    , gvAlignment :: Word64            -- ^ The logarithm base 2 of the variable's requested alignment, plus 1
    , gvSection :: Word64              -- ^ If non-zero, the 1-based section index in the table of @MODULE_SECTION_NAME@.
    , gvVisibility :: Visibility       -- ^ If present, an encoding of the visibility of this variable
    , gvThreadLocal :: ThreadLocalMode -- ^ If present, an encoding of the thread local storage mode of the variable
    , gvUnnamedAddr :: Bool            -- ^ If present and non-zero, indicates that the variable has @unnamed_addr@
    , gvExternallyInitialized :: Bool
    , gvDLLStorageClass :: DLLStorageClass -- ^ If present, an encoding of the DLL storage class of this variable
    , gvComdat :: Word64 -- ???
    }
  | Function
    { fType :: Word64                  -- ^ The type index of the function type describing this function
    , fCallingConv :: CallingConv
    , fIsProto :: Bool                 -- ^ Non-zero if this entry represents a declaration rather than a definition
    , fLinkage :: Linkage
    , fParamAttrs :: Word64            -- ^ If nonzero, the 1-based parameter attribute index into the table of @PARAMATTR_CODE_ENTRY@ entries.
    , fAlignment :: Word64
    , fSection :: Word64               -- ^ If non-zero, the 1-based section index in the table of @MODULE_CODE_SECTIONNAME@ entries.
    , fVisibility :: Visibility
    , fGC :: Word64                    -- ^ If present and nonzero, the 1-based garbage collector index in the table of @MODULE_CODE_GCNAME@ entries.
    , fUnnamedAddr :: Bool             -- ^ If present and non-zero, indicates that the function has @unnamed_addr@.
    , fPrologueData :: Word64          -- ^ If non-zero, the value index of the prologue data for this function, plus 1.
    , fDLLStorageClass :: DLLStorageClass -- ^ An encoding of the DLL storage class of this function.
    , fComdat :: Word64                -- ^ An encoding of the COMDAT of this function
    , fPrefixData :: Word64            -- ^ If non-zero, the value index of the prefix data for this function, plus 1.
    , fPersonalityFn :: Word64         -- ^ If non-zero, the value index of the personality function for this function, plus 1.
    }
  -- | The @PURGEVALS@ record (code 10) resets the module-level value list to the
  -- size given by the single operand value. Module-level value list items are added
  -- by @GLOBALVAR@, @FUNCTION@, and @ALIAS@ records.  After a @PURGEVALS@
  -- record is seen, new value indices will start from the given *numvals* value.
  | PurgeVals Word64
  -- | The ``GCNAME`` record (code 11) contains a variable number of values
  -- representing the bytes of a single garbage collector name string. There should
  -- be one ``GCNAME`` record for each garbage collector name referenced in function
  -- ``gc`` attributes within the module. These records can be referenced by 1-based
  -- index in the *gc* fields of ``FUNCTION`` records.
  | GCName String
  | Comdat                             -- TODO: selection_kind, name ???
  | VSTOffset                          --
  -- | The @ALIAS@ record (code 9) marks the definition of an alias.
  | Alias
    { aType :: Word64                  -- ^ The type index of the alias
    , aVal  :: Word64                  -- ^ The value index of the aliased value
    , aLinkage :: Linkage
    , aVisibility :: Visibility
    , aDLLStorageClass :: DLLStorageClass
    }
  | MetadataValues Word64
  deriving Show
