{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Data.BitCode.LLVM.ToBitCode where

import Data.BitCode (NBitCode, mkBlock, mkRec, mkEmptyRec)
import Data.BitCode.LLVM
import Data.BitCode.LLVM.Value as V (Const(..), Value(..), Symbol, symbolValue)
import Data.BitCode.LLVM.Type  as T (Ty(..), subTypes)

import Data.BitCode.LLVM.IDs.Blocks
import qualified Data.BitCode.LLVM.Codes.Identification as IC
import qualified Data.BitCode.LLVM.Codes.Module         as MC
import qualified Data.BitCode.LLVM.Codes.Type           as TC
import qualified Data.BitCode.LLVM.Codes.Constants      as CC

import Data.BitCode.LLVM.Classes.ToSymbols
import Data.List (elemIndex, sort, sortOn, groupBy, nub)
import Data.Function (on)

import Data.Maybe (fromMaybe)
import Data.Word  (Word64)

import Data.Bits ((.|.), shift)

--------------------------------------------------------------------------------
-- Turn things into NBitCode.
--
class ToNBitCode a where
  toBitCode :: a -> [NBitCode]

-- | Rerturns the Identification Block
instance ToNBitCode Ident where
  toBitCode (Ident name epoch)
    = pure $ mkBlock IDENTIFICATION [ mkRec IC.STRING name
                                    , mkRec IC.EPOCH epoch
                                    ]

instance (ToNBitCode a) => ToNBitCode [a] where
  toBitCode = concatMap toBitCode

instance {-# OVERLAPPING #-} ToNBitCode [Ty] where
  toBitCode tys
    = pure $ mkBlock TYPE_NEW (numEntryRec:map mkTypeRec tys)
    where numEntryRec :: NBitCode
          numEntryRec = mkRec TC.NUMENTRY (length tys)
          mkTypeRec :: Ty -> NBitCode
          mkTypeRec T.Void     = mkEmptyRec TC.VOID
          mkTypeRec T.Float    = mkEmptyRec TC.FLOAT
          mkTypeRec T.Double   = mkEmptyRec TC.DOUBLE
          mkTypeRec T.Label    = mkEmptyRec TC.LABEL
          mkTypeRec T.Opaque   = mkEmptyRec TC.OPAQUE
          mkTypeRec (T.Int w)  = mkRec TC.INTEGER [w]
          mkTypeRec (T.Ptr s t)= mkRec TC.POINTER [lookupIndex tys t, s]
          mkTypeRec T.Half     = mkEmptyRec TC.HALF
          mkTypeRec (T.Array n t)= mkRec TC.ARRAY [n, lookupIndex tys t]
          mkTypeRec (T.Vector n t) = mkRec TC.VECTOR [n, lookupIndex tys t]
          mkTypeRec T.X86Fp80  = mkEmptyRec TC.X86_FP80
          mkTypeRec T.Fp128    = mkEmptyRec TC.FP128
          mkTypeRec T.Metadata = mkEmptyRec TC.METADATA
          mkTypeRec X86Mmx     = mkEmptyRec TC.X86_MMX
          mkTypeRec (T.StructAnon p ts) = mkRec TC.STRUCT_ANON ((if p then 1 else 0 :: Int):map (lookupIndex tys) ts)
          mkTypeRec (T.StructName n) = mkRec TC.STRUCT_NAME n
          mkTypeRec (T.StructNamed p ts) = mkRec TC.STRUCT_NAMED ((if p then 1 else 0 :: Int):map (lookupIndex tys) ts)
          mkTypeRec (T.Function vargs t ts) = mkRec TC.FUNCTION ((if vargs then 1 else 0::Int):map (lookupIndex tys) (t:ts))
          mkTypeRec T.Token    = mkEmptyRec TC.TOKEN

lookupIndex :: (Eq a, Show a, Integral b) => [a] -> a -> b
lookupIndex xs x = case elemIndex x xs of
  Just i -> fromIntegral i
  Nothing -> error $ "Unable to find " ++ show x ++ " in " ++ show xs


instance ToNBitCode Module where
  toBitCode m@(Module{..})
    = pure . mkBlock MODULE $
      [ mkRec MC.VERSION [mVersion] ] ++
      toBitCode allTypes ++
      [ mkRec MC.TRIPLE t | Just t <- [mTriple] ] ++
      [ mkRec MC.DATALAYOUT dl | Just dl <- [mDatalayout] ] ++
      [ mkBlock CONSTANTS $ mkConstRecs constants ] ++
      map mkGlobalRec globals ++
      map mkFunctionRec functions ++
      []
    -- = pure $ mkBlock MODULE [ {- Record: Version 1 -}
    --                         , {- Block: ParamAttrGroup 10 -}
    --                         , {- Block: ParamAttr 9 -}
    --                         , {- Block: Types 17 -}
    --                         , {- Record: Triple 2 -}
    --                         , {- Record: Datalayout 3 -}
    --                         , {- Record: GlobalVar 7 -}, ...
    --                         , {- Record: Function 8 -}, ...
    --                         , {- Record: VSTOffset 13 -}
    --                         , {- Block: Constants 11 -}
    --                         , {- Block: Metadata 15 -}, ...
    --                         , {- Block: Function 12 -}
    --                         , {- Block: SymTab 14 -}
    --                         ]
    where
      -- globals
      globals = [g | g@(Global{}) <- map symbolValue mValues]
      -- functions
      functions = [f | f@(V.Function{}) <- map symbolValue mValues]
      -- TODO: aliases??
      -- constants
      constants = sortOn cTy [c | c@(Constant{}) <- map symbolValue mValues]
      -- The types used in the module.
      topLevelTypes = nub . sort . map ty . symbols $ m
      -- all types contains all types including those that types reference. (e.g. i8** -> i8**, i8*, and i8)
      allTypes = nub . sort $ topLevelTypes ++ concatMap subTypes topLevelTypes


      mkConstRecs :: [Value] -> [NBitCode]
      mkConstRecs consts = concatMap f (groupBy ((==) `on` cTy) consts)
        where f [] = []
              f ((Constant t c):cs) = (mkRec CC.CST_CODE_SETTYPE (lookupIndex allTypes t :: Int)):mkConstRec c:map (mkConstRec . cConst) cs
      mkConstRec :: Const -> NBitCode
      mkConstRec Null = mkEmptyRec CC.CST_CODE_NULL
      mkConstRec Undef = mkEmptyRec CC.CST_CODE_UNDEF
      mkConstRec (V.Int n) = mkRec CC.CST_CODE_INTEGER n
      mkConstRec (WideInt ns) = mkRec CC.CST_CODE_WIDE_INTEGER ns
      -- TODO: Float encoding?
--      mkConstRec (Float f) = mkRect CC.CST_CODE_FLOAT f
      -- TODO: Support aggregates (lookup value numbers in Constants? + Globals + Functions?)
--      mkConstRec (Aggregate valueNs)
      mkConstRec (String s) = mkRec CC.CST_CODE_STRING s
      mkConstRec (CString s) = mkRec CC.CST_CODE_CSTRING s
      -- XXX BinOp, Cast, Gep, Select, ExtractElt, InsertElt, ShuffleVec, Cmp, InlineAsm, ShuffleVecEx,
      mkConstRec (InboundsGep t symbls) = mkRec CC.CST_CODE_CE_INBOUNDS_GEP ((lookupIndex allTypes t :: Int):map (lookupIndex constants . symbolValue) symbls)
      -- XXX BlockAddress, Data, InlineAsm

      bool :: (Integral a) => Bool -> a
      bool x = if x then 1 else 0

      fromEnum' :: (Enum a, Integral b) => a -> b
      fromEnum' = fromIntegral . fromEnum

      mkGlobalRec :: Value -> NBitCode
      mkGlobalRec (Global{..}) = mkRec MC.GLOBALVAR [ lookupIndex allTypes gPointerType
                                                    , 1 .|. shift (bool gIsConst) 1 .|. shift gAddressSpace 2
                                                    , fromMaybe 0 ((+1) . lookupIndex constants <$> gInit)
                                                    , fromEnum' gLinkage
                                                    , gParamAttrs
                                                    , gSection
                                                    , fromEnum' gVisibility
                                                    , fromEnum' gThreadLocal
                                                    , bool gUnnamedAddr
                                                    , bool gExternallyInitialized
                                                    , fromEnum' gDLLStorageClass
                                                    , gComdat
                                                    ]

      mkFunctionRec :: Value -> NBitCode
      mkFunctionRec (V.Function{..}) = mkRec MC.FUNCTION [ lookupIndex allTypes fType
                                                         , fromEnum' fCallingConv
                                                         , bool fIsProto
                                                         , fromEnum' fLinkage
                                                         , fParamAttrs
                                                         , fAlignment
                                                         , fSection
                                                         , fromEnum' fVisibility
                                                         , fGC
                                                         , bool fUnnamedAddr
                                                         , fPrologueData
                                                         , fromEnum' fDLLStorageClass
                                                         , fComdat
                                                         , fPrefixData
                                                         , fPersonalityFn
                                                         ]


-- X = [NBlock 13 [NRec 1 [65,80,80,76,69,95,49,95,55,48,51,46,48,46,51,49,95,48],NRec 2 [0]]
--     ,NBlock 8
--      [NRec 1 [1]
--      ,NBlock 10 [NRec 3 [1,4294967295,0,18,0,26,0,33,4,100,105,115,97,98,108,101,45,116,97,105,108,45,99,97,108,108,115,0,102,97,108,115,101,0,4,108,101,115,115,45,112,114,101,99,105,115,101,45,102,112,109,97,100,0,102,97,108,115,101,0,4,110,111,45,102,114,97,109,101,45,112,111,105,110,116,101,114,45,101,108,105,109,0,116,114,117,101,0,3,110,111,45,102,114,97,109,101,45,112,111,105,110,116,101,114,45,101,108,105,109,45,110,111,110,45,108,101,97,102,0,4,110,111,45,105,110,102,115,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,110,111,45,110,97,110,115,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,115,116,97,99,107,45,112,114,111,116,101,99,116,111,114,45,98,117,102,102,101,114,45,115,105,122,101,0,56,0,4,116,97,114,103,101,116,45,99,112,117,0,99,111,114,101,50,0,4,116,97,114,103,101,116,45,102,101,97,116,117,114,101,115,0,43,99,120,49,54,44,43,102,120,115,114,44,43,109,109,120,44,43,115,115,101,44,43,115,115,101,50,44,43,115,115,101,51,44,43,115,115,115,101,51,0,4,117,110,115,97,102,101,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,117,115,101,45,115,111,102,116,45,102,108,111,97,116,0,102,97,108,115,101,0]
--                 ,NRec 3 [2,4294967295,4,100,105,115,97,98,108,101,45,116,97,105,108,45,99,97,108,108,115,0,102,97,108,115,101,0,4,108,101,115,115,45,112,114,101,99,105,115,101,45,102,112,109,97,100,0,102,97,108,115,101,0,4,110,111,45,102,114,97,109,101,45,112,111,105,110,116,101,114,45,101,108,105,109,0,116,114,117,101,0,3,110,111,45,102,114,97,109,101,45,112,111,105,110,116,101,114,45,101,108,105,109,45,110,111,110,45,108,101,97,102,0,4,110,111,45,105,110,102,115,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,110,111,45,110,97,110,115,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,115,116,97,99,107,45,112,114,111,116,101,99,116,111,114,45,98,117,102,102,101,114,45,115,105,122,101,0,56,0,4,116,97,114,103,101,116,45,99,112,117,0,99,111,114,101,50,0,4,116,97,114,103,101,116,45,102,101,97,116,117,114,101,115,0,43,99,120,49,54,44,43,102,120,115,114,44,43,109,109,120,44,43,115,115,101,44,43,115,115,101,50,44,43,115,115,101,51,44,43,115,115,115,101,51,0,4,117,110,115,97,102,101,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,117,115,101,45,115,111,102,116,45,102,108,111,97,116,0,102,97,108,115,101,0]]
--      ,NBlock 9 [NRec 2 [1],NRec 2 [2]]
--      ,NBlock 17 [NRec 1 [14],NRec 7 [8],NRec 11 [13,0],NRec 8 [1,0],NRec 7 [32],NRec 8 [0,0],NRec 8 [4,0],NRec 21 [0,3,3,5],NRec 8 [6,0],NRec 21 [1,3,4],NRec 8 [8,0],NRec 16 [],NRec 8 [3,0],NRec 8 [5,0],NRec 2 []]
--      ,NRec 2 [120,56,54,95,54,52,45,97,112,112,108,101,45,109,97,99,111,115,120,49,48,46,49,49,46,48]
--      ,NRec 3 [101,45,109,58,111,45,105,54,52,58,54,52,45,102,56,48,58,49,50,56,45,110,56,58,49,54,58,51,50,58,54,52,45,83,49,50,56]
--      ,NRec 7 [1,3,4,9,1,0,0,0,1,0,0,0]
--      ,NRec 8 [6,0,0,0,1,0,0,0,0,0,0,0,0,0,0]
--      ,NRec 8 [8,0,1,0,2,0,0,0,0,0,0,0,0,0,0]
--      ,NRec 13 [470]
--      ,NBlock 11 [NRec 1 [1],NRec 9 [104,101,108,108,111,32,119,111,114,108,100,10],NRec 1 [3],NRec 4 [2],NRec 4 [4]]
--      ,NBlock 15 [NRec 2 [3,4]
--                 ,NRec 1 [80,73,67,32,76,101,118,101,108]
--                 ,NRec 2 [3,5]
--                 ,NRec 3 [1,2,3]
--                 ,NRec 1 [65,112,112,108,101,32,76,76,86,77,32,118,101,114,115,105,111,110,32,55,46,51,46,48,32,40,99,108,97,110,103,45,55,48,51,46,48,46,51,49,41]
--                 ,NRec 3 [5]
--                 ,NRec 4 [108,108,118,109,46,109,111,100,117,108,101,46,102,108,97,103,115]
--                 ,NRec 10 [3]
--                 ,NRec 4 [108,108,118,109,46,105,100,101,110,116]
--                 ,NRec 10 [5]]
--      ,NBlock 15 [NRec 6 [0,100,98,103]
--                 ,NRec 6 [1,116,98,97,97]
--                 ,NRec 6 [2,112,114,111,102]
--                 ,NRec 6 [3,102,112,109,97,116,104]
--                 ,NRec 6 [4,114,97,110,103,101]
--                 ,NRec 6 [5,116,98,97,97,46,115,116,114,117,99,116]
--                 ,NRec 6 [6,105,110,118,97,114,105,97,110,116,46,108,111,97,100]
--                 ,NRec 6 [7,97,108,105,97,115,46,115,99,111,112,101]
--                 ,NRec 6 [8,110,111,97,108,105,97,115]
--                 ,NRec 6 [9,110,111,110,116,101,109,112,111,114,97,108]
--                 ,NRec 6 [10,108,108,118,109,46,109,101,109,46,112,97,114,97,108,108,101,108,95,108,111,111,112,95,97,99,99,101,115,115]
--                 ,NRec 6 [11,110,111,110,110,117,108,108]
--                 ,NRec 6 [12,100,101,114,101,102,101,114,101,110,99,101,97,98,108,101]
--                 ,NRec 6 [13,100,101,114,101,102,101,114,101,110,99,101,97,98,108,101,95,111,114,95,110,117,108,108]
--                 ,NRec 6 [14,109,97,107,101,46,105,109,112,108,105,99,105,116]
--                 ,NRec 6 [15,117,110,112,114,101,100,105,99,116,97,98,108,101]
--                 ,NRec 6 [16,105,110,118,97,114,105,97,110,116,46,103,114,111,117,112]
--                 ,NRec 6 [17,97,108,105,103,110]]
--      ,NBlock 12 [NRec 1 [1]
--                 ,NBlock 11 [NRec 1 [3]
--                            ,NRec 2 []
--                            ,NRec 1 [4]
--                            ,NRec 20 [1,2,0,3,8,3,8]]
--                 ,NRec 19 [3,3,4,67]
--                 ,NRec 19 [5,3,4,68]
--                 ,NRec 44 [2,6,3,0]
--                 ,NRec 44 [1,5,4,0]
--                 ,NRec 34 [0,32768,8,10,3]
--                 ,NRec 10 [5]
--                 ,NBlock 14 [NRec 1 [7,97,114,103,118]
--                            ,NRec 1 [6,97,114,103,99]]]
--      ,NBlock 14 [NRec 1 [2,112,114,105,110,116,102]
--                 ,NRec 3 [1,448,109,97,105,110]
--                 ,NRec 1 [0,46,115,116,114]]]]
