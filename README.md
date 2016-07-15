# Data.BitCode

Pure haskell support for reading and writing the BitCode Container Format.

-- 

# TODO

- [ ] Rename `read` to `expect`, where it's used in expectance.
- [ ] Rename `parse` to `read`
- [ ] Factor out `whenM` so these nasty `if ... then ... else return ()` can be dropped.
- [ ] Add abbreviation width knowledge to the writer.
- [ ] Add support for returning the abbreviaion id, for defAbbrev to be used in abbrevRecord, in the writer monad.
- [ ] Add support for obtaining the current abbreviation map to the reader monad (push, pop?).
- [ ] Add support to obtain the current abbreviationWidth from the reader monad (push, pop?).
- [ ] Extensive haddock documentaiton
- [ ] Add doc tests to make verify it works; and for documentation.
- [ ] Compare LLVM  IR/*.h diffs and add @since@ flags.
- [x] Rename From/ToBitCode to From/ToBits
