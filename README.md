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

# TODO: 
- [ ] build sample app that uses more than one function. (e.g. call out to a second function)
- [ ] Unify ANON_STRUCT and NAMED_STRUCT. If it has a name it's named, otherwise it's anonynmous.
- [ ] Put CallingConv into the function type, instead of the function. The issue is that we might
      bitcast a function, but loose the relevant information on how to call the casted result, if
      we do not store the relevant information in the result of a cast op.
- [ ] Resolve all ForwardReferences (A few more FwdRef's were introduced that are non yet all resolved).

# High Prio

- [ ] Support all instructions
- [ ] Applicative Module Construction interface
- [ ] Some form of support for Metadata
- [ ] Some form of support for Attributes
- [ ] Support USELIST
- [ ] Extensive haddock documentaiton
- [ ] Convert VBR to Signed VBR. Signed VBR's value is shifted by 1, and the low bit indicates if negative or not.
  - [x] Used in `CST_CODE_INTEGER`, `CST_CODE_WIDE_INTEGER`
  - [ ] and `CONSTANTS_BLOCK` records.
  - [ ] Also for phi instruction operands in `MODULE_CODE_VERSION` 1.

# Medium Prio

- [ ] Rename `XXX_CODE_YYY` into `YYY`, they usually live in the corresponding module anyway.
- [ ] Drop the Call Markers file and unify with BitFlags (including inAllocaMask, SwiftErrorMask, ...)
- [ ] Rename `read` to `expect`, where it's used in expectance.
- [ ] Rename `parse` to `read`
- [ ] Use use Control.Monad (when, unless) where possible.
- [ ] Add doc tests to make verify it works; and for documentation.
      Need to figure out how to do this properly. No idea yet.

# Low Prio

- [ ] Support Function Block VSTs
- [ ] Writer: Add support for VST Offsets to the writer (since 3.8).
      (Not sure if this will be mandatory in the future).
      See: https://reviews.llvm.org/D12536
      The linked document: https://drive.google.com/file/d/0B036uwnWM6RWdnBLakxmeDdOeXc/view describes
      the reasoning. We likely does *not* support ThinLTO without this.
- [ ] Writer: Support BasicBlock Labels (`VST_CODE_BBENTRY`)
- [ ] Compare LLVM  IR/*.h diffs and add @since@ flags.
      Not sure how to do this properly.
- [ ] Abbreviation support
      This is not strictly necessary as it's only a compression scheme. Would still be nice to have.
      However, the logic for the encoding would probably have to live in Data.BitCode.LLVM, as the
      information about the actuall types is erazed when transfered through NBitCode.
  - [ ] Add abbreviation width knowledge to the writer.
  - [ ] Add support for returning the abbreviaion id, for defAbbrev to be used in abbrevRecord, in the writer monad.
  - [ ] Add support for obtaining the current abbreviation map to the reader monad (push, pop?).
  - [ ] Add support to obtain the current abbreviationWidth from the reader monad (push, pop?).
