{-# LANGUAGE TupleSections #-}
module Data.BitCode.Playground where

import Prelude hiding (writeFile)
import Data.BitCode
import Data.BitCode.Writer.Monad
import Data.BitCode.Writer (emitTopLevel)
import Data.BitCode.Writer.Combinators
import Data.BitCode.IDs.FixedAbbrev
import qualified Data.BitCode.IDs.StandardBlock as SBlock
import qualified Data.BitCode.LLVM.IDs.Blocks as LLVMBlock
import qualified Data.BitCode.LLVM.Codes.Identification as Ident
import qualified Data.BitCode.LLVM.Codes.Module as Mod
import qualified Data.BitCode.LLVM.Codes.Type as Ty

-- emitSimpleBlock = ppBitCodeWriter $ emitFixed 2 (fromEnum ENTER_SUBBLOCK) >> emitVBR 8 (3 :: Int)

mkBlock :: (Enum a) => a -> Int -> [Block] -> Block
mkBlock id = Block (fromEnum id)
mkLit :: (Enum a) => a -> Op
mkLit = Lit . fromIntegral . fromEnum
mkW64 :: (Enum a) => a -> Field
mkW64 = W64 . fromIntegral . fromEnum
mkUnabbrevRecord :: (Enum a, Integral b) => a -> [b] -> Block
mkUnabbrevRecord e ops = UnabbrevRecord (fromIntegral . fromEnum $ e) (map fromIntegral ops)
mkAbbrevRecord :: (Enum a) => a -> [Field] -> Block
mkAbbrevRecord c fs = AbbrevRecord (fromIntegral . fromEnum $ c) fs

helloWorld :: [Block]
helloWorld = [ mkBlock LLVMBlock.IDENTIFICATION 5 [ DefAbbrevRecord [mkLit Ident.STRING, Enc Arr, Enc Char6] -- this is given id 4
                                                  , mkAbbrevRecord 4 (Vbr 6 18:map Chr "APPLE_1_703.0.31_0")
                                                  , DefAbbrevRecord [mkLit Ident.EPOCH, Enc (VBR 6)]         -- this is given id 5
                                                  , mkAbbrevRecord 5 [Vbr 6 0]]
             , mkBlock LLVMBlock.MODULE         3 [ mkUnabbrevRecord Mod.VERSION [1]
                                                  -- empty block info block
                                                  , mkBlock SBlock.BLOCKINFO 2 []
                                                  -- no idea how to understand PARAMATTR yet.
--                                                  , mkBlock LLVMBlock.PARAMATTR_GROUP 3 [ mkUnabbrevRecord 
--                                                                                        ]
--                                                  , mkBlock LLVMBlock.PARAMATTR 3 [ ]
                                                  , mkBlock LLVMBlock.TYPE_NEW 4 [ mkUnabbrevRecord Ty.NUMENTRY [14]
                                                                                 , mkUnabbrevRecord Ty.INTEGER  [8]
--                                                                                 , mkUnabbrevRecord
                                                                                 ]
                                                  , Block 13 3 []
                                                  ]
             ]


-- emitHelloWorld = ppBitCodeWriter . emitTopLevel $ helloWorld
writeHelloWorld fp = writeFile fp . withHeader True . emitTopLevel $ helloWorld
