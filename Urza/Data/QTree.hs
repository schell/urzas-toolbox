module Urza.Data.QTree (
    QTree(..),
    QLeaf(..),
    empty,
    insert,
    elems,
    keys,
    bounds,
    leaves,
    queryWith,
    queryLeavesWith,
    queryPoint
) where

import Urza.Math.Rectangle
import Urza.Types
import Data.Maybe
import Linear

type Extents a = (QTree a, QTree a, QTree a, QTree a)

data QTree a = QTree { _qtBounds   :: BoundingBox
                     , _qtBranches :: Maybe (Extents a)
                     , _qtLeaves   :: [QLeaf a]
                     } deriving (Show)

data QLeaf a = QLeaf { _qlBounds :: BoundingBox
                     , _qlEntry  :: a
                     } deriving (Show)


insert :: BoundingBox -> a -> QTree a -> QTree a
insert bb a q = fromMaybe q $ tryInsert bb a q

queryPoint :: V2 Double -> QTree a -> [a]
queryPoint p = queryWith (pointOnRect p)

queryWith :: (BoundingBox -> Bool) -> QTree a -> [a]
queryWith f t = map leafVal $ queryLeavesWith f t

queryLeavesWith :: (BoundingBox -> Bool) -> QTree a -> [QLeaf a]
queryLeavesWith f (QTree bb Nothing ls) =
    if f bb then catMaybes $ map (queryLeafWith f) ls else []
queryLeavesWith f (QTree bb (Just qs) ls) =
    if f bb
      then concatMap (queryLeavesWith f) (exToList qs) ++ catMaybes (map (queryLeafWith f) ls)
      else []

queryLeafWith :: (BoundingBox -> Bool) -> QLeaf a -> Maybe (QLeaf a)
queryLeafWith f (QLeaf bb a) = if f bb then Just $ QLeaf bb a else Nothing

leaves :: QTree a -> [QLeaf a]
leaves (QTree _ Nothing ls) = ls
leaves (QTree _ (Just (q1,q2,q3,q4)) ls) = ls ++ concatMap leaves [q1,q2,q3,q4]

elems :: QTree a -> [a]
elems (QTree _ Nothing ls) = map leafVal ls
elems (QTree _ (Just (q1,q2,q3,q4)) ls) = (map leafVal ls) ++ concatMap elems [q1,q2,q3,q4]

keys :: QTree a -> [BoundingBox]
keys (QTree bb Nothing ls) = bb:map leafBox ls
keys (QTree bb (Just (q1,q2,q3,q4)) ls) = bb:map leafBox ls ++ concatMap keys [q1,q2,q3,q4]

bounds :: QTree a -> BoundingBox
bounds (QTree bb _ _) = bb

empty :: BoundingBox -> QTree a
empty = emptyBranch

tryInsert :: BoundingBox -> a -> QTree a -> Maybe (QTree a)
tryInsert bb1 a (QTree bb2 Nothing ls)
    | bb2 `containsRect` bb1 = tryInsert bb1 a $ QTree bb2 (Just $ boundsToQuads bb2) ls
    | otherwise = Nothing
tryInsert bb1 a (QTree bb2 (Just bs) ls)
    | bb2 `containsRect` bb1 = Just $ case tryInsertingInBranches bb1 a bs of
                                   Nothing -> QTree bb2 (Just bs) (leaf bb1 a:ls)
                                   mBs     -> QTree bb2 mBs ls
    | otherwise = Nothing

tryInsertingInBranches :: BoundingBox -> a -> Extents a -> Maybe (Extents a)
tryInsertingInBranches bb a (q1,q2,q3,q4) =
    case catMaybes mQuads of
        [] -> Nothing
        _  -> Just (q1',q2',q3',q4')
      where quads = [q1,q2,q3,q4]
            mQuads = map (tryInsert bb a) quads
            [q1',q2',q3',q4'] = zipWith fromMaybe quads mQuads

boundsToQuads :: BoundingBox -> (QTree a, QTree a, QTree a, QTree a)
boundsToQuads bb = (q1',q2',q3',q4')
    where [q1',q2',q3',q4'] = map emptyBranch [q1,q2,q3,q4]
          (q1,q2,q3,q4) = quadrants bb

leaf :: BoundingBox -> a -> QLeaf a
leaf bb a = QLeaf bb a

leafVal :: QLeaf a -> a
leafVal (QLeaf _ a) = a

leafBox :: QLeaf a -> BoundingBox
leafBox (QLeaf bb _) = bb

emptyBranch :: BoundingBox -> QTree a
emptyBranch bb = QTree bb Nothing []

exToList :: (a,a,a,a) -> [a]
exToList (a,b,c,d) = [a,b,c,d]
