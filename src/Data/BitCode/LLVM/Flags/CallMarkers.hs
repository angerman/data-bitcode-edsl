module Data.BitCode.LLVM.Flags.CallMarkers where
-- | Markers and flags for call instruction.
data CallMarkers
  = CALL_TAIL -- 0
  | CALL_CCONV -- 1
  | CALL_UNUSED2 | CALL_UNUSED3 | CALL_UNUSED4 | CALL_UNUSED5  | CALL_UNUSED6
  | CALL_UNUSED7 | CALL_UNUSED8 | CALL_UNUSED9 | CALL_UNUSED10  | CALL_UNUSED11
  | CALL_UNUSED12 | CALL_UNUSED13 
  | CALL_MUSTTAIL -- 14
  | CALL_EXPLICIT_TYPE -- 15
  | CALL_NOTAIL -- 16
  -- | Call has optional fast-math-flags.
  | CALL_FMF -- 17
