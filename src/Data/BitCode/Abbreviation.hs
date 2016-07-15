module Data.BitCode.Abbreviation
  ( addAbbrev, lookupAbbrev
  , addGlobalAbbrev, lookupGlobalAbbrev
  , AbbrevMap
  , GlobalAbbrevMap
  )
where

import Data.BitCode
import Data.Maybe (fromMaybe)

newtype AbbrevMap = AbbrevMap [(Code, Block)] deriving Show
newtype GlobalAbbrevMap = GlobalAbbrevMap [(BlockID, AbbrevMap)] deriving Show

instance Monoid AbbrevMap where
  mempty = AbbrevMap []
  (AbbrevMap m) `mappend` (AbbrevMap n) = AbbrevMap (m ++ n)

instance Monoid GlobalAbbrevMap where
  mempty = GlobalAbbrevMap []
  (GlobalAbbrevMap m) `mappend` (GlobalAbbrevMap n) = GlobalAbbrevMap (m ++ n)

lookupGlobalAbbrev :: GlobalAbbrevMap -> BlockID -> AbbrevMap
lookupGlobalAbbrev (GlobalAbbrevMap g) blockId = fromMaybe mempty (lookup blockId g)

addGlobalAbbrev :: GlobalAbbrevMap -> BlockID -> Block -> GlobalAbbrevMap
addGlobalAbbrev (GlobalAbbrevMap g) blockId block = GlobalAbbrevMap g'
  where g' = go g blockId block
        go :: [(BlockID, AbbrevMap)] -> BlockID -> Block -> [(BlockID, AbbrevMap)]
        go [] id b = [(blockId, addAbbrev mempty block)]
        go (gb@(id', bs):g') id block | id == id' = (id, addAbbrev bs block):go g' id block
                                      | otherwise = gb:go g' id block

lookupAbbrev :: AbbrevMap -> Code -> Maybe Block
lookupAbbrev (AbbrevMap m) = flip lookup m

addAbbrev :: AbbrevMap -> Block -> AbbrevMap
addAbbrev (AbbrevMap m) r@(DefAbbrevRecord ops) = AbbrevMap $ (nextId,r):m
  where nextId = 1 + foldr max 3 (map fst m)

