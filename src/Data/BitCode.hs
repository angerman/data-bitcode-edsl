{-# LANGUAGE RankNTypes #-}
module Data.BitCode where

import Data.Word (Word32, Word64)

-- * Bits
type Bit = Bool
type Bits = [Bool]

-- * BitCode
type BlockID = Int
type Code    = Int

--- Bit Codes ------------------------------------------------------------------
-- see BitCodes.h (e.g. http://llvm.org/docs/doxygen/html/BitCodes_8h_source.html)

-- | Bit Code data values can be 64bit wide.
type Val = Word64
data EncVal = Fixed !Val   -- code 1 fixed value
            | VBR   !Val   -- code 2 vbr value
            | Arr          -- code 3 Array -- the documentation sais, an Array needs to be followed by an op.
                                           -- when reading an array, the first is a vbr6 field indicating the length.
            | Char6        -- code 4 6-bit char
            | Blob         -- code 5 note: the value for this is: [vbr6:val,pad32bit,8bit array,pad32bit]
            deriving Show

-- | Operators for abbreviated records, are encoded as either literal (1) or encoded value (0).
data Op = Lit !Val          -- [1,vbr8:val]
        | Enc !EncVal       -- [0,f3:enc(,vbr5:val)?], vbr5 value only if given.
        deriving Show

-- | The Fields contained in an abbreviated record can be one of the following.
data Field = Vbr !Int !Word64
           | Fix !Int !Word64
           | Chr !Char
           | W64 !Val         -- Literal values. These are not bein emitted.
                              -- WARN: this is somewhat a hack, to make parsing and writing identical to id,
                              --       without having to track abbreviations in the writer and ensure the
                              --       abbreviated record matches the def abbrev. This could be considered
                              --       a TODO, as it would be an improvement to enforce the that AbbrevRecord
                              --       matches the actuall DefAbbrev.
           deriving Show

-- | Bit Code Data consists of a series of blocks. Their interpretation is dependent
-- on the container they are in.  The top level blocks are emitted with an abbreviation
-- width of 2. This allows the following four block types, which allow to define any
-- other set of blocks.
data Block
  -- | Combine ENTER_SUBBLOCK(1) with END_BLOCK(0)
  -- Layout: [1,vbr8:id,vbr4:newabbrevlen,<align32bits>,32bit:blocklen,<blocklen * words>,0,<align32bits>]
  -- 1 and 0 are vbr(current abbrev len); starting with 2 at the top level.
  = Block { blockId        :: !Int     -- ^ id
          , blockAbbrevLen :: !Int     -- ^ abbrev len
          , blockBody      :: ![Block] -- ^ body
          }
  -- | A abbreviation definition record. Layout: [2,vbr5:#ops,op0,op1,...]
  | DefAbbrevRecord { defRecordOps :: ![Op] }
  -- | An unabbreviated record. Layout: [3,vbr6:code,vbr6:#ops,vbr6:op0,...]
  | UnabbrevRecord { uRecordCode :: !Word64  -- ^ code         encoded vbr6
                   , uRecordOps :: ![Word64] -- ^ generic ops, encoded vbr6
                   }
  -- | An abbreviated record. Layout [<abbrevcode>, fields, ...]
  | AbbrevRecord { aRecordCode :: !Int
                 , aRecordFields :: ![Field]
                 }
    deriving Show

