# Data.BitCode

Pure haskell support for reading and writing the BitCode Container Format.

-- 

# TODO

- [ ] Rename `read` to `expect`, where it's used in expectance.
- [ ] Rename `parse` to `read`
- [ ] Factor out `whenM` so these nasty `if ... then ... else return ()` can be dropped. -- use Control.Monad (when, unless)
- [ ] Add abbreviation width knowledge to the writer.
- [ ] Add support for returning the abbreviaion id, for defAbbrev to be used in abbrevRecord, in the writer monad.
- [ ] Add support for obtaining the current abbreviation map to the reader monad (push, pop?).
- [ ] Add support to obtain the current abbreviationWidth from the reader monad (push, pop?).
- [ ] Extensive haddock documentaiton
- [ ] Add doc tests to make verify it works; and for documentation.
- [ ] Compare LLVM  IR/*.h diffs and add @since@ flags.
- [x] Rename From/ToBitCode to From/ToBits
- [ ] Convert VBR to Signed VBR. Signed VBR's value is shifted by 1, and the low bit indicates if negative or not.
      Used in `CST_CODE_INTEGER`, `CST_CODE_WIDE_INTEGER` and `CONSTANTS_BLOCK` records.
      Also for phi instruction operands in `MODULE_CODE_VERSION` 1.

- [ ] Module parsing needs to happen in sequence.
- [ ] write parseBlock, that can parse Abbrev, *or* subblock.
  - [x] parse data-layout
  - [x] parse triple
  - [x] parse types
  - [x] parse globals -> valueList     globals depend on types
  - [x] parse constants -> valueList   constants depend on types
  - [x] can parse VST after reading VSTOffset, but don't *really* need vst. It's just name mapping. (might need for external import though :-))
  - [x] turn attr into the parseAttr fn and hook into parseValue.
  - [x] turn attrGroup into parseAttrGroup fn and hook into parseValue
  - [x] parse block metadata - not *that* important. Just metadata.
  - [x] parse metadata kind
  - [x] The VST for a *function* can be parsed later. It's probably just giving sybols to the arguments. These can live perfectly with
        simple ids. (The instructions just have *id*s, which *might* have symbols attached, or not.)
  - [x] Parse function block
  - [x] Add symbol :: Maybe String, to potentially named objects; parse VST and update the corresponding values.
  - [x] Properly parse basic blocks.
  - [ ] Add Metadata to Module
  - [ ] Add Attributes to Module
  - [ ] !Replace the initId value the corresponding symbol in Global
- [ ] LLVMWriter
  - [x] Build basic ToBitCode Class that turns stuff into NBitCode
  - [x] Build denormalize :: NBitCode -> BitCode
  - [x] Test write pipeline (e.g. produce a .bc file from a ToBitCode a)
  - [ ] Collect Types and put them into a type table
  - [ ] Build Values
    - [ ] Build Global (Global Value, Functions, External Functions) Records
    - [ ] Build Constants block. (Group by Type)
  - [ ] Generate Function Blocks
    - [ ] Lookup Type Idx's and replace types with their references.
    - [ ] Lookup Value Idx's and replace them with their references.
