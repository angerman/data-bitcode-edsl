module Annotated.HelloWorld
  (helloWorld)
where

import Data.BitCode

--- hello module, in (non-located) BitCode AST
helloWorld :: [BitCode]
helloWorld =
  [ Block
    { blockId = 13
    , blockAbbrevLen = 5
    , blockBody =
      [ DefAbbrevRecord {defRecordOps = [Lit 1,Enc Arr,Enc Char6]}
       , AbbrevRecord { aRecordCode = 4
                     , aRecordFields = [W64 1,Len 18,Chr 'A',Chr 'P',Chr 'P',Chr 'L',Chr 'E',Chr '_',Chr '1',Chr '_',Chr '7',Chr '0',Chr '3',Chr '.',Chr '0',Chr '.',Chr '3',Chr '1',Chr '_',Chr '0']}
      , DefAbbrevRecord {defRecordOps = [Lit 2,Enc (VBR 6)]}
      , AbbrevRecord {aRecordCode = 5, aRecordFields = [W64 2,Vbr 6 0]}]}
  , Block
    { blockId = 8
    , blockAbbrevLen = 3
    , blockBody =
      [ UnabbrevRecord {uRecordCode = 1, uRecordOps = [1]}                                                 -- Version: 1
      , Block
        { blockId = 0
        , blockAbbrevLen = 2
        , blockBody =
          [ UnabbrevRecord {uRecordCode = 1, uRecordOps = [14]}
          , DefAbbrevRecord {defRecordOps = [Enc (Fixed 3),Enc (VBR 8),Enc Arr,Enc (Fixed 8)]}
          , DefAbbrevRecord {defRecordOps = [Lit 1,Enc (VBR 8),Enc Arr,Enc (Fixed 7)]}
          , DefAbbrevRecord {defRecordOps = [Lit 1,Enc (VBR 8),Enc Arr,Enc Char6]}
          , DefAbbrevRecord {defRecordOps = [Lit 2,Enc (VBR 8),Enc Arr,Enc Char6]}
          , UnabbrevRecord {uRecordCode = 1, uRecordOps = [11]}
          , DefAbbrevRecord {defRecordOps = [Lit 1,Enc (Fixed 4)]}
          , DefAbbrevRecord {defRecordOps = [Lit 4,Enc (VBR 8)]}
          , DefAbbrevRecord {defRecordOps = [Lit 11,Enc (Fixed 4),Enc (Fixed 4),Enc (VBR 8)]}
          , DefAbbrevRecord {defRecordOps = [Lit 2]}
          , UnabbrevRecord {uRecordCode = 1, uRecordOps = [12]}
          , DefAbbrevRecord {defRecordOps = [Lit 20,Enc (VBR 6),Enc (Fixed 4),Enc (VBR 4),Enc (Fixed 1)]}
          , DefAbbrevRecord {defRecordOps = [Lit 2,Enc (VBR 6),Enc (VBR 6),Enc (Fixed 4)]}
          , DefAbbrevRecord {defRecordOps = [Lit 2,Enc (VBR 6),Enc (VBR 6),Enc (Fixed 4),Enc (Fixed 7)]}
          , DefAbbrevRecord {defRecordOps = [Lit 3,Enc (VBR 6),Enc (Fixed 4),Enc (Fixed 4)]}
          , DefAbbrevRecord {defRecordOps = [Lit 10]}
          , DefAbbrevRecord {defRecordOps = [Lit 10,Enc (VBR 6)]}
          , DefAbbrevRecord {defRecordOps = [Lit 15]}
          , DefAbbrevRecord {defRecordOps = [Lit 43,Enc (Fixed 1),Enc (Fixed 4),Enc Arr,Enc (VBR 6)]}]}
      , Block
        { blockId = 10
        , blockAbbrevLen = 3
        , blockBody =
          [ UnabbrevRecord {uRecordCode = 3, uRecordOps = [1,4294967295,0,18,0,26,0,33,4,100,105,115,97,98,108,101,45,116,97,105,108,45,99,97,108,108,115,0,102,97,108,115,101,0,4,108,101,115,115,45,112,114,101,99,105,115,101,45,102,112,109,97,100,0,102,97,108,115,101,0,4,110,111,45,102,114,97,109,101,45,112,111,105,110,116,101,114,45,101,108,105,109,0,116,114,117,101,0,3,110,111,45,102,114,97,109,101,45,112,111,105,110,116,101,114,45,101,108,105,109,45,110,111,110,45,108,101,97,102,0,4,110,111,45,105,110,102,115,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,110,111,45,110,97,110,115,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,115,116,97,99,107,45,112,114,111,116,101,99,116,111,114,45,98,117,102,102,101,114,45,115,105,122,101,0,56,0,4,116,97,114,103,101,116,45,99,112,117,0,99,111,114,101,50,0,4,116,97,114,103,101,116,45,102,101,97,116,117,114,101,115,0,43,99,120,49,54,44,43,102,120,115,114,44,43,109,109,120,44,43,115,115,101,44,43,115,115,101,50,44,43,115,115,101,51,44,43,115,115,115,101,51,0,4,117,110,115,97,102,101,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,117,115,101,45,115,111,102,116,45,102,108,111,97,116,0,102,97,108,115,101,0]}
          -- 1: Function
          --    - ATTR_KIND_NO_UNWIND
          --    - ATTR_KIND_STACK_PROTECT
          --    - ATTR_KIND_UW_TABLE
          --    - disable-tail-calls: false
          --    - less-precise-fpmad: false
          --    - no-frame-pointer-elim: true
          --    - no-frame-pointer-elim-non-leaf
          --    - no-infs-fp-math: false
          --    - no-nans-fp-math: false
          --    - stack-protector-buffer-size: 8
          --    - target-cpu: core2
          --    - target-features: +cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+ssse3
          --    - unsafe-fp-math: false
          --    - use-soft-float: false
          , UnabbrevRecord {uRecordCode = 3, uRecordOps = [2,4294967295,4,100,105,115,97,98,108,101,45,116,97,105,108,45,99,97,108,108,115,0,102,97,108,115,101,0,4,108,101,115,115,45,112,114,101,99,105,115,101,45,102,112,109,97,100,0,102,97,108,115,101,0,4,110,111,45,102,114,97,109,101,45,112,111,105,110,116,101,114,45,101,108,105,109,0,116,114,117,101,0,3,110,111,45,102,114,97,109,101,45,112,111,105,110,116,101,114,45,101,108,105,109,45,110,111,110,45,108,101,97,102,0,4,110,111,45,105,110,102,115,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,110,111,45,110,97,110,115,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,115,116,97,99,107,45,112,114,111,116,101,99,116,111,114,45,98,117,102,102,101,114,45,115,105,122,101,0,56,0,4,116,97,114,103,101,116,45,99,112,117,0,99,111,114,101,50,0,4,116,97,114,103,101,116,45,102,101,97,116,117,114,101,115,0,43,99,120,49,54,44,43,102,120,115,114,44,43,109,109,120,44,43,115,115,101,44,43,115,115,101,50,44,43,115,115,101,51,44,43,115,115,115,101,51,0,4,117,110,115,97,102,101,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,117,115,101,45,115,111,102,116,45,102,108,111,97,116,0,102,97,108,115,101,0]}
          -- 2: Function
          --    - disable-tail-calls: false
          --    - less-precise-fpmad: false
          --    - no-frame-pointer-elim: true
          --    - no-frame-pointer-elim-non-leaf
          --    - no-infs-fp-math: false
          --    - no-nans-fp-math: false
          --    - stack-protector-buffer-size: 8
          --    - target-cpu: core2
          --    - target-features: +cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+ssse3
          --    - unsafe-fp-math: false
          --    - use-soft-float: false
          ]}
      , Block
        { blockId = 9
        , blockAbbrevLen = 3
        , blockBody =
          [ UnabbrevRecord { uRecordCode = 2, uRecordOps = [1] } -- ParamAttr 0 = 1
          , UnabbrevRecord { uRecordCode = 2, uRecordOps = [2] } -- ParamAttr 1 = 2
          ]}
      , Block
        { blockId = 17
        , blockAbbrevLen = 4
        , blockBody =
          [ DefAbbrevRecord {defRecordOps = [Lit 8,Enc (Fixed 4),Lit 0]}
          , DefAbbrevRecord {defRecordOps = [Lit 21,Enc (Fixed 1),Enc Arr,Enc (Fixed 4)]}
          , DefAbbrevRecord {defRecordOps = [Lit 18,Enc (Fixed 1),Enc Arr,Enc (Fixed 4)]}
          , DefAbbrevRecord {defRecordOps = [Lit 19,Enc Arr,Enc Char6]}
          , DefAbbrevRecord {defRecordOps = [Lit 20,Enc (Fixed 1),Enc Arr,Enc (Fixed 4)]}
          , DefAbbrevRecord {defRecordOps = [Lit 11,Enc (VBR 8),Enc (Fixed 4)]}
          , UnabbrevRecord {uRecordCode = 1, uRecordOps = [14]}                                            -- 14 elements
          , UnabbrevRecord {uRecordCode = 7, uRecordOps = [8]}                                             --  0: i8
          , AbbrevRecord {aRecordCode = 9, aRecordFields = [W64 11,Vbr 8 13,Fix 4 0]}                      --  1: <13 x i8>
          , AbbrevRecord {aRecordCode = 4, aRecordFields = [W64 8,Fix 4 1,W64 0]}                          --  2: *<13 x i8>
          , UnabbrevRecord {uRecordCode = 7, uRecordOps = [32]}                                            --  3: i32
          , AbbrevRecord {aRecordCode = 4, aRecordFields = [W64 8,Fix 4 0,W64 0]}                          --  4: *i8
          , AbbrevRecord {aRecordCode = 4, aRecordFields = [W64 8,Fix 4 4,W64 0]}                          --  5: **i8
          , AbbrevRecord {aRecordCode = 5, aRecordFields = [W64 21,Fix 1 0,Len 3,Fix 4 3,Fix 4 3,Fix 4 5]} --  6: (i32, **i8) -> i32
          , AbbrevRecord {aRecordCode = 4, aRecordFields = [W64 8,Fix 4 6,W64 0]}                          --  7: *((i32,**i8) -> i32)
          , AbbrevRecord {aRecordCode = 5, aRecordFields = [W64 21,Fix 1 1,Len 2,Fix 4 3,Fix 4 4]}         --  8: (*i8...) -> i32
          , AbbrevRecord {aRecordCode = 4, aRecordFields = [W64 8,Fix 4 8,W64 0]}                          --  9: *((*i8...) -> i32)
          , UnabbrevRecord {uRecordCode = 16, uRecordOps = []}                                             -- 10: metadata type
          , AbbrevRecord {aRecordCode = 4, aRecordFields = [W64 8,Fix 4 3,W64 0]}                          -- 11: *i32
          , AbbrevRecord {aRecordCode = 4, aRecordFields = [W64 8,Fix 4 5,W64 0]}                          -- 12: ***i8
          , UnabbrevRecord {uRecordCode = 2, uRecordOps = []}]}                                            -- 13: void
      , UnabbrevRecord {uRecordCode = 2, uRecordOps = [120,56,54,95,54,52,45,97,112,112,108                -- triple: x86_64-apple-macosx10.11.0
                                                      ,101,45,109,97,99,111,115,120,49,48,46,49,49,46,48]}
      , UnabbrevRecord {uRecordCode = 3, uRecordOps = [101,45,109,58,111,45,105,54,52,58,54                -- data layout: e-m:o-i64:64-f80:128-n8:16:32:64-S128
                                                      ,52,45,102,56,48,58,49,50,56,45,110,56
                                                      ,58,49,54,58,51,50,58,54,52,45,83,49,50,56]}
      , DefAbbrevRecord {defRecordOps = [Lit 7,Enc (Fixed 1),Enc (VBR 6),Enc (VBR 6),Enc (Fixed 5),Enc (Fixed 1),Lit 0]}
      , UnabbrevRecord {uRecordCode = 7, uRecordOps = [1,3,4,9,1,0,0,0,1,0,0,0]}                           -- Global Var:
                                                                                                           --  ptr type    : <13 x i8>
                                                                                                           --  isConst     : isConstant (1), explicitType(2)
                                                                                                           --    explicitType -> AddressSpace = 3 >> 2
                                                                                                           --    _            ->  AddressSpace = address space of ptr type
                                                                                                           --                     prt type = get underlying type of ptr type.
                                                                                                           --  initId      : 3 = 4 - 1 -- (the new GV)
                                                                                                           --                3 is "Hello World\n"
                                                                                                           --  linkage     : ExternalWeak
                                                                                                           --  paramattr   : 1 lokup in ATTRS
                                                                                                           --  section     : 0
                                                                                                           --  visibility  : Default
                                                                                                           --  threadLocal : 0
                                                                                                           --  unnamed_addr: True
                                                                                                           --  externallyInitialized: 0
                                                                                                           --  storageclass: Default
                                                                                                           --  comdat      : 0
       , UnabbrevRecord {uRecordCode = 8, uRecordOps = [6,0,0,0,1,0,0,0,0,0,0,0,0,0,0]}                    -- Function
                                                                                                           --  type        : (i32, i8**) -> i32
                                                                                                           --  cconv       : C
                                                                                                           --  isproto     : False
                                                                                                           --  linkage     : External
                                                                                                           --  paramattrs  : 1 lookup in ATTRS
                                                                                                           --  alignment   : 0
                                                                                                           --  section     : 0
                                                                                                           --  visibility  : Default
                                                                                                           --  gc          : 0
                                                                                                           --  unnamed_addr: False
                                                                                                           --  prologuedata: 0
                                                                                                           --  storageclass: Default
                                                                                                           --  comdat      : 0
                                                                                                           --  prefixdata  : 0
                                                                                                           --  personality : 0

      , UnabbrevRecord {uRecordCode = 8, uRecordOps = [8,0,1,0,2,0,0,0,0,0,0,0,0,0,0]}                     -- Function
                                                                                                           --  type        : (i8*...) -> i32
                                                                                                           --  cconv       : C
                                                                                                           --  isproto     : True
                                                                                                           --  linkage     : External
                                                                                                           --  paramattrs  : 2 lookup in ATTRS
                                                                                                           --  alignment   : 0
                                                                                                           --  section     : 0
                                                                                                           --  visibility  : Default
                                                                                                           --  gc          : 0
                                                                                                           --  unnamed_addr: False
                                                                                                           --  prologuedata: 0
                                                                                                           --  storageclass: Default
                                                                                                           --  comdat      : 0
                                                                                                           --  prefixdata  : 0
                                                                                                           --  personality : 0

      -- VALUE LIST:
      -- - 0 Global Var
      -- - 1 Func
      -- - 2 Func

      , DefAbbrevRecord {defRecordOps = [Lit 13,Enc (Fixed 32)]}
      , AbbrevRecord {aRecordCode = 5, aRecordFields = [W64 13,Fix 32 470]}                                -- VSTOffset: 470 (ValueSymbleTableOffset)
                                                                                                           -- this is the offset of the VST in 32bit
                                                                                                           -- width from here.  So we can jump to the
                                                                                                           -- VST and parse it first, so that we get
                                                                                                           -- the first few items for the Value List.
                                                                                                           -- E.g. before parsing the constants, which
                                                                                                           -- are just added to the list.
      , Block
        { blockId = 11                                                                                     -- CONSTANTS
        , blockAbbrevLen = 4
        , blockBody =
          [ DefAbbrevRecord {defRecordOps = [Lit 7,Enc Arr,Enc (Fixed 3)]}
          , DefAbbrevRecord {defRecordOps = [Lit 8,Enc Arr,Enc (Fixed 8)]}
          , DefAbbrevRecord {defRecordOps = [Lit 9,Enc Arr,Enc (Fixed 7)]}
          , DefAbbrevRecord {defRecordOps = [Lit 9,Enc Arr,Enc Char6]}
          , AbbrevRecord {aRecordCode = 4, aRecordFields = [W64 1,Fix 4 1]}                                -- set type: <13 x i8>
          , AbbrevRecord {aRecordCode = 10, aRecordFields = [W64 9,Len 12,Fix 7 104,Fix 7 101,Fix 7 108    -- 3: CString: "Hello World\n"
                                                            ,Fix 7 108,Fix 7 111,Fix 7 32,Fix 7 119
                                                            ,Fix 7 111,Fix 7 114,Fix 7 108,Fix 7 100
                                                            ,Fix 7 10]}
          , AbbrevRecord {aRecordCode = 4, aRecordFields = [W64 1,Fix 4 3]}                                -- set type: i32
          , AbbrevRecord {aRecordCode = 5, aRecordFields = [W64 4,Vbr 8 2]}                                -- 4: Integer: 2
          , AbbrevRecord {aRecordCode = 5, aRecordFields = [W64 4,Vbr 8 4]}]}                              -- 5: Integer: 4
      , Block
        { blockId = 15                                                                                     -- METADATA -- this is a stack like op. named node replaces the top two items into a named item.
        , blockAbbrevLen = 3
        , blockBody =
          [ DefAbbrevRecord {defRecordOps = [Lit 1,Enc Arr,Enc (Fixed 8)]}
          , DefAbbrevRecord {defRecordOps = [Lit 4,Enc Arr,Enc (Fixed 8)]}
          , UnabbrevRecord {uRecordCode = 2, uRecordOps = [3,4]}                                           --  0: value: 4 :: i32
          , AbbrevRecord {aRecordCode = 4, aRecordFields = [W64 1,Len 9,Fix 8 80,Fix 8 73,Fix 8 67,Fix 8 32--  1: metadata string: PIC Level
                                                           ,Fix 8 76,Fix 8 101,Fix 8 118,Fix 8 101,Fix 8 108]}
          , UnabbrevRecord {uRecordCode = 2, uRecordOps = [3,5]}                                           --  2: value: 5 :: i32
          , UnabbrevRecord {uRecordCode = 3, uRecordOps = [1,2,3]}                                         --  3: node: [4, PIC Level, 5]
          , AbbrevRecord {aRecordCode = 4, aRecordFields = [W64 1,Len 41,Fix 8 65,Fix 8 112,Fix 8 112      --  4: metada string:
                                                           ,Fix 8 108,Fix 8 101,Fix 8 32,Fix 8 76          --     Apple LLVM version 7.3.0 (clang-703.0.31)
                                                           ,Fix 8 76,Fix 8 86,Fix 8 77,Fix 8 32,Fix 8 118
                                                           ,Fix 8 101,Fix 8 114,Fix 8 115,Fix 8 105
                                                           ,Fix 8 111,Fix 8 110,Fix 8 32,Fix 8 55,Fix 8 46
                                                           ,Fix 8 51,Fix 8 46,Fix 8 48,Fix 8 32,Fix 8 40
                                                           ,Fix 8 99,Fix 8 108,Fix 8 97,Fix 8 110,Fix 8 103
                                                           ,Fix 8 45,Fix 8 55,Fix 8 48,Fix 8 51,Fix 8 46
                                                           ,Fix 8 48,Fix 8 46,Fix 8 51,Fix 8 49,Fix 8 41]}
          , UnabbrevRecord {uRecordCode = 3, uRecordOps = [5]}                                             --  5: node [Apple LLVM version 7.3.0 (clang-703.0.31)]
          , AbbrevRecord {aRecordCode = 5, aRecordFields = [W64 4,Len 17,Fix 8 108,Fix 8 108,Fix 8 118     --  6: metadata name:
                                                           ,Fix 8 109,Fix 8 46,Fix 8 109,Fix 8 111         --     llvm.module.flags
                                                           ,Fix 8 100,Fix 8 117,Fix 8 108,Fix 8 101
                                                           ,Fix 8 46,Fix 8 102,Fix 8 108,Fix 8 97
                                                           ,Fix 8 103,Fix 8 115]}
          , UnabbrevRecord {uRecordCode = 10, uRecordOps = [3]}                                            --  7: metadata named node [[4, PIC Level, 5]]
          , AbbrevRecord {aRecordCode = 5, aRecordFields = [W64 4,Len 10,Fix 8 108,Fix 8 108,Fix 8 118     --  8: metadata.name:
                                                           ,Fix 8 109,Fix 8 46,Fix 8 105,Fix 8 100         --     llvm.ident
                                                           ,Fix 8 101,Fix 8 110,Fix 8 116]}
          , UnabbrevRecord {uRecordCode = 10, uRecordOps = [5]}]}                                          --  9: metadata named node [[Apple LLVM ...]]
      , Block
        { blockId = 15                                                                                     -- METADATA
        , blockAbbrevLen = 3
        , blockBody =
          [ UnabbrevRecord {uRecordCode = 6, uRecordOps = [0,100,98,103]}                                  -- md kind:  0 - dbg
          , UnabbrevRecord {uRecordCode = 6, uRecordOps = [1,116,98,97,97]}                                -- md kind:  1 - tbaa
          , UnabbrevRecord {uRecordCode = 6, uRecordOps = [2,112,114,111,102]}                             -- md kind:  2 - prof
          , UnabbrevRecord {uRecordCode = 6, uRecordOps = [3,102,112,109,97,116,104]}                      -- md kind:  3 - fpmath
          , UnabbrevRecord {uRecordCode = 6, uRecordOps = [4,114,97,110,103,101]}                          -- md kind:  4 - rang
          , UnabbrevRecord {uRecordCode = 6, uRecordOps = [5,116,98,97,97,46,115,116,114,117,99,116]}      -- md kind:  5 - tbaa.struct
          , UnabbrevRecord {uRecordCode = 6, uRecordOps = [6,105,110,118,97,114,105,97                     -- md kind:  6 - invariant.load
                                                          ,110,116,46,108,111,97,100]}
          , UnabbrevRecord {uRecordCode = 6, uRecordOps = [7,97,108,105,97,115,46,115,99,111,112,101]}     -- md kind:  7 - alias.scope
          , UnabbrevRecord {uRecordCode = 6, uRecordOps = [8,110,111,97,108,105,97,115]}                   -- md kind:  8 - noalias
          , UnabbrevRecord {uRecordCode = 6, uRecordOps = [9,110,111,110,116,101,109,112,111,114,97,108]}  -- md kind:  9 - nontemporal
          , UnabbrevRecord {uRecordCode = 6, uRecordOps = [10,108,108,118,109,46,109,101,109,46,112,97,114 -- md kind: 10 - llvm.mem.parallel_loop_access
                                                          ,97,108,108,101,108,95,108,111,111,112,95,97,99
                                                          ,99,101,115,115]}
          , UnabbrevRecord {uRecordCode = 6, uRecordOps = [11,110,111,110,110,117,108,108]}                -- md kind: 11 - nonnull
          , UnabbrevRecord {uRecordCode = 6, uRecordOps = [12,100,101,114,101,102,101,114                  -- md kind: 12 - dereferenceable
                                                          ,101,110,99,101,97,98,108,101]}
          , UnabbrevRecord {uRecordCode = 6, uRecordOps = [13,100,101,114,101,102,101,114                  -- md kind: 13 - dereferenceable_or_null
                                                          ,101,110,99,101,97,98,108,101
                                                          ,95,111,114,95,110,117,108,108]}
          , UnabbrevRecord {uRecordCode = 6, uRecordOps = [14,109,97,107,101,46,105,109                    -- md kind: 14 - make.implicit
                                                          ,112,108,105,99,105,116]}
          , UnabbrevRecord {uRecordCode = 6, uRecordOps = [15,117,110,112,114,101,100,105                  -- md kind: 15 - unpredictable
                                                          ,99,116,97,98,108,101]}
          , UnabbrevRecord {uRecordCode = 6, uRecordOps = [16,105,110,118,97,114,105,97,110                -- md kind: 16 - invariant.group
                                                          ,116,46,103,114,111,117,112]}
          , UnabbrevRecord {uRecordCode = 6, uRecordOps = [17,97,108,105,103,110]}]}                       -- md kind: 17 - align
      , Block
        { blockId = 12                                                                                     -- FUNCTION
        , blockAbbrevLen = 4
        , blockBody =
          [ UnabbrevRecord {uRecordCode = 1, uRecordOps = [1]}                                             -- Declare blocks: 1
          , Block
            { blockId = 11                                                                                 -- - CONSTANTS
            , blockAbbrevLen = 4
            , blockBody =
              [ AbbrevRecord {aRecordCode = 4, aRecordFields = [W64 1,Fix 4 3]}                            -- - set type: i32
              , UnabbrevRecord {uRecordCode = 2, uRecordOps = []}                                          -- - null
              , AbbrevRecord {aRecordCode = 4, aRecordFields = [W64 1,Fix 4 4]}                            -- - set type: *i8
              , UnabbrevRecord {uRecordCode = 20, uRecordOps = [1,2,0,3,8,3,8]}]}                          -- - Inbound GEP: 1,2,0,3,8,3,8

                                                                                                           -- Values so far:
                                                                                                           -- Glob:0 - glob for 3 - .str
                                                                                                           --      1 - Fn         - main (offset: 448)
                                                                                                           --      2 - Fn         - printf
                                                                                                           -- Mod: 3 - "Hello World\n" :: <13 x i8>
                                                                                                           --      4 - 2 :: i32 (signed: 1)
                                                                                                           --      5 - 4 :: i32 (signed: 2)
                                                                                                           -- FArgs: 6 - i32  - argc from VST
                                                                                                           --        7 - i8** - argv from VST
                                                                                                           ----
                                                                                                           -- Fn   8 - null :: i32
                                                                                                           --      9 - Inbound GEP (1,2,0,3,8,3,8) :: *i8
                                                                                                           --
                                                                                                           --
                                                                                                           --
                                                                                                           --
          , UnabbrevRecord {uRecordCode = 19, uRecordOps = [3,3,4,67]}                                     -- ALLOCA [instty, opty, op, align]
                                                                                                           -- Return ptr to instty, with size for (opty,op)
                                                                                                           --
                                                                                                           -- mask 00000111  5: InAllocMask
                                                                                                           --                6: ExplicitTypeMask
                                                                                                           --                7: SwiftErrorMask
                                                                                                           -- 67:  11000010
                                                                                                           --      1100     -1
                                                                                                           --      01      = 2
                                                                                                           --      2^2     = 4 (align)
                                                                                                           --
                                                                                                           -- %10 :: i32*
                                                                                                           -- %10 = ALLOCA i32 (val 4->1), align 4
                                                                                                           --
          , UnabbrevRecord {uRecordCode = 19, uRecordOps = [5,3,4,68]}                                     -- %11 :: i8***
                                                                                                           -- %11 = ALLOCA i32 (val 4->1), align 8
                                                                                                           --
                                                                                                           -- Store (44) [ptrty,ptr,val,align[,vol]]
          , UnabbrevRecord {uRecordCode = 44, uRecordOps = [2,6,3,0]}                                      -- STORE (12-2=10) i32* %1
                                                                                                           --       <- (12-6 = 6) %argc
                                                                                                           --       align 3-1 = 2 -> 2^2 = 4
          , UnabbrevRecord {uRecordCode = 44, uRecordOps = [1,5,4,0]}                                      -- STORE (12-1=11) i8*** %2
                                                                                                           --       <- (12-5 < 0 -> 7) %argv
                                                                                                           --       align 4-1 = 3 -> 2^3 = 8
          , UnabbrevRecord {uRecordCode = 34, uRecordOps = [0,32768,8,10,3]}                               -- #12 = CALL attr:0
                                                                                                           --      cc:32768 - 0000000000000010)
                                                                                                           --                               ^
                                                                                                           --                       Explicit Type
                                                                                                           --      fnty:8 = (*i8,...) -> i32
                                                                                                           --      fnid:12-10 = 2 -> printf
                                                                                                           --      args...:12-3 = 9 -> inbounds GEP (1,2,0,3,8,3,8) :: i8*
                                                                                                           --      GEP: 1 - pointee type: <13 x i8>
                                                                                                           --           2 - elTy0 : *<13 x i8>
                                                                                                           --           0 - elVal0: .str
                                                                                                           --           3 - elTy1 : i32
                                                                                                           --           8 - elVal1: null
                                                                                                           --           3 - elTy2 : i32
                                                                                                           --           8 - elVal2: null
          , AbbrevRecord {aRecordCode = 9, aRecordFields = [W64 10,Vbr 6 5]}                               -- RET (13-5=8) null :: i32
          , Block
            { blockId = 14                                                                                 -- - VALUE SYMBOL TABLE
            , blockAbbrevLen = 4
            , blockBody =
              [ AbbrevRecord {aRecordCode = 6, aRecordFields = [W64 1,Vbr 8 7,Len 4                        -- - 7: argv (valueid:name)
                                                               ,Chr 'a',Chr 'r',Chr 'g',Chr 'v']}
              , AbbrevRecord {aRecordCode = 6, aRecordFields = [W64 1,Vbr 8 6,Len 4                        -- - 6: argc
                                                               ,Chr 'a',Chr 'r',Chr 'g',Chr 'c']}]}]}
      , Block
        { blockId = 14                                                                                     -- VALUE SYMBOL TABLE
        , blockAbbrevLen = 4
        , blockBody =
          [ DefAbbrevRecord {defRecordOps = [Lit 3,Enc (VBR 8),Enc (VBR 8),Enc Arr,Enc (Fixed 8)]}
          , DefAbbrevRecord {defRecordOps = [Lit 3,Enc (VBR 8),Enc (VBR 8),Enc Arr,Enc (Fixed 7)]}
          , DefAbbrevRecord {defRecordOps = [Lit 3,Enc (VBR 8),Enc (VBR 8),Enc Arr,Enc Char6]}
          , AbbrevRecord {aRecordCode = 6, aRecordFields = [W64 1,Vbr 8 2,Len 6                            -- entry (id: 2, value: printf)
                                                           ,Chr 'p',Chr 'r',Chr 'i',Chr 'n',Chr 't',Chr 'f']}
          , AbbrevRecord {aRecordCode = 10, aRecordFields = [W64 3,Vbr 8 1,Vbr 8 448,Len 4                 -- fnentry: (id:1, offset: 448, value: main)
                                                            ,Chr 'm',Chr 'a',Chr 'i',Chr 'n']}
          , AbbrevRecord {aRecordCode = 6, aRecordFields = [W64 1,Vbr 8 0,Len 4                            -- entry (id: 0, value: .str)
                                                           ,Chr '.',Chr 's',Chr 't',Chr 'r']}]}]}]
