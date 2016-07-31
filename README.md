# Data.BitCode

Pure haskell support for reading and writing the BitCode Container Format.

---

# Tools

To see the block list (as seen by the bitcode reader), use

```
$ llvm-bcanalyzer file.bc -dump|less
```

to verify the dissassembly looks as expected, use

```
$ llvm-dis -f file.bc -show-annotations && less file.ll
```

and to turn the produced bitcode into a binary, use

```
$ llc file.bc -filetype=obj # will produce file.o
```

to subsequently link with a linker.

`clang` however can do all the linking from a bc file onwards:

```
$ clang file.bc && ./a.out
```

---

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
  - [x] !Replace the initId value the corresponding symbol in Global
- [x] LLVMWriter
  - [x] Build basic ToBitCode Class that turns stuff into NBitCode
  - [x] Build denormalize :: NBitCode -> BitCode
  - [x] Test write pipeline (e.g. produce a .bc file from a ToBitCode a)
  - [x] Collect Types and put them into a type table
  - [x] Build Values
    - [x] Build Global (Global Value, Functions, External Functions) Records
    - [x] Build Constants block. (Group by Type)
  - [x] Generate Function Blocks
    - [x] Lookup Type Idx's and replace types with their references.
    - [x] Lookup Value Idx's and replace them with their references.
  - [x] Build VSTs (necessary to properly be able to link external functions.)

# Medium Prio:

- [ ] Rename `XXX_CODE_YYY` into `YYY`, they usually live in the corresponding module anyway.
- [ ] Drop the Call Markers file and unify with BitFlags (including inAllocaMask, SwiftErrorMask, ...)

# Low Prio:

- [ ] Writer: Add support for VST Offsets to the writer (since 3.8).
      (Not sure if this will be mandatory in the future).
      See: https://reviews.llvm.org/D12536
      The linked document: https://drive.google.com/file/d/0B036uwnWM6RWdnBLakxmeDdOeXc/view describes
      the reasoning. We likely does *not* support ThinLTO without this.
- [ ] Writer: Support BasicBlock Labels (`VST_CODE_BBENTRY`)
