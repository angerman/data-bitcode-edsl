module HelloMod (hello) where

import Text.LLVM.AST

-- * Example module (hello.c)
--
-- Hello Module in Text.LLVM.IR AST format.
hello :: Module
hello = Module
      { modDataLayout = [ LittleEndian
                        , Mangling MachOMangling
                        , IntegerSize 64 64 Nothing
                        , FloatSize 80 128 Nothing
                        , NativeIntSize [8,16,32,64]
                        , StackAlign 128
                        ]
      , modTypes = []
      , modNamedMd = [ NamedMd {nmName = "llvm.ident", nmValues = [1]}
                     , NamedMd {nmName = "llvm.module.flags", nmValues = [0]}]
      , modUnnamedMd = [ UnnamedMd { umIndex = 0
                                   , umValues = [ Just (ValMdValue (Typed { typedType = PrimType (Integer 32)
                                                                          , typedValue = ValInteger 1}))
                                                , Just (ValMdString "PIC Level")
                                                , Just (ValMdValue (Typed { typedType = PrimType (Integer 32)
                                                                          , typedValue = ValInteger 2}))]
                                   , umDistinct = False
                                   }
                       , UnnamedMd { umIndex = 1
                                   , umValues = [Just (ValMdString "Apple LLVM version 7.3.0 (clang-703.0.31)")]
                                   , umDistinct = False
                                   }
                       ]
      , modGlobals = [ Global { globalSym = Symbol ".str"
                              , globalAttrs = GlobalAttrs { gaLinkage = Just Private
                                                          , gaConstant = True
                                                          }
                              , globalType = Array 13 (PrimType (Integer 8))
                              , globalValue = Just (ValString "hello world\n\NUL")
                              , globalAlign = Just 1
                              }
                     ]
      , modDeclares = [ Declare { decRetType = PrimType (Integer 32)
                                , decName = Symbol "printf"
                                , decArgs = [PtrTo (PrimType (Integer 8))]
                                , decVarArgs = True
                                }
                      ]
      , modDefines = [ Define { defAttrs = FunAttrs { funLinkage = Nothing, funGC = Nothing }
                              , defRetType = PrimType (Integer 32)
                              , defName = Symbol "main"
                              , defArgs = [ Typed { typedType = PrimType (Integer 32)
                                                  , typedValue = Ident "argc" }
                                          , Typed { typedType = PtrTo (PtrTo (PrimType (Integer 8)))
                                                  , typedValue = Ident "argv" }
                                          ]
                              , defVarArgs = False
                              , defSection = Nothing
                              , defBody = [BasicBlock { bbLabel = Just (Anon 0)
                                                      , bbStmts = [ Result (Ident "1") (Alloca (PrimType (Integer 32)) Nothing (Just 4)) []
                                                                  , Result (Ident "2") (Alloca (PtrTo (PtrTo (PrimType (Integer 8)))) Nothing (Just 8)) []
                                                                  , Effect (Store
                                                                            (Typed { typedType = PrimType (Integer 32)
                                                                                   , typedValue = ValIdent (Ident "argc") })
                                                                            (Typed { typedType = PtrTo (PrimType (Integer 32))
                                                                                   , typedValue = ValIdent (Ident "1")})
                                                                            (Just 4)) []
                                                                  , Effect (Store
                                                                             (Typed { typedType = PtrTo (PtrTo (PrimType (Integer 8)))
                                                                                    , typedValue = ValIdent (Ident "argv")})
                                                                             (Typed { typedType = PtrTo (PtrTo (PtrTo (PrimType (Integer 8))))
                                                                                    , typedValue = ValIdent (Ident "2")})
                                                                             (Just 8)) []
                                                                  , Result (Ident "3") (Call False (PtrTo (FunTy
                                                                                                           (PrimType (Integer 32))
                                                                                                           [PtrTo (PrimType (Integer 8))]
                                                                                                           True))
                                                                                         (ValSymbol (Symbol "printf"))
                                                                                         [Typed { typedType = PtrTo (PrimType (Integer 8)),
                                                                                                  typedValue = ValConstExpr (ConstGEP
                                                                                                                             True
                                                                                                                             [ Typed { typedType = PtrTo (Array 13 (PrimType (Integer 8)))
                                                                                                                                     , typedValue = ValSymbol (Symbol ".str")}
                                                                                                                             , Typed { typedType = PrimType (Integer 32)
                                                                                                                                     , typedValue = ValInteger 0}
                                                                                                                             , Typed { typedType = PrimType (Integer 32)
                                                                                                                                     , typedValue = ValInteger 0}])}]) []
                                                                  , Effect (Ret (Typed { typedType = PrimType (Integer 32)
                                                                                       , typedValue = ValInteger 0})) []
                                                                  ]}]}]
      , modInlineAsm = []
      , modAliases = []
      }
