{-#
LANGUAGE NoImplicitPrelude,
         TypeFamilies
#-}

module Data.PairSet (
  PairSet,
  left,      right,        
  lefts,     rights,       
  leftElem,  rightElem
  elem,      elems,        
  lookRight, lookLeft,     
  delete,    fromList
  switch,    insert,       
  psUnion,   psDifference, 
  psIntersection
  ) where

import Prelude (Ord,Bool, (.), ($), uncurry, flip, const, not, Show, show, (++))

import Data.Functor
import Control.Applicative
import Control.Monad

import Data.Semigroup
import Data.Maybe (Maybe(..))
import Data.Foldable (Foldable,foldl',foldr, null)
import qualified Data.Foldable as F
import Data.Monoid (Monoid,mempty,mappend)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

import Data.Containers
import Data.MonoTraversable



data PairSet a b = PairSet {
        left :: !(Map a (Set b)),
        right :: !(Map b (Set a))
        }

instance (Ord a, Ord b) => Monoid (PairSet a b) where
  mempty = PairSet mempty mempty
  mappend = psUnion
instance (Ord a,Ord b) => Semigroup (PairSet a b) where
  (<>) = psUnion

type instance Element (PairSet a b) = (a,b)

instance (Ord a,Ord b) => MonoFoldable (PairSet a b) where
  ofoldMap f x   = F.foldMap f $ elems x
  ofoldr   f z x = foldr  f z  $ elems x
  ofoldl'  f z x = foldl' f z  $ elems x
  ofoldr1Ex f x = ofoldr1Ex f $ elems x
  ofoldl1Ex' f x = ofoldr1Ex f $ elems x

instance (Ord a, Show a, Ord b, Show b) => Show (PairSet a b) where
  show m = "fromList "++show (elems m)

instance (Ord a, Ord b) => GrowingAppend (PairSet a b)

instance (Ord a,Ord b) => SetContainer (PairSet a b) where
   type ContainerKey (PairSet a b) = (a,b)
   member = elem
   notMember x s = not $ x `elem` s
   union = psUnion
   difference = psDifference
   intersection = psIntersection
   keys = elems

instance (Ord a, Ord b) => IsSet (PairSet a b) where
   insertSet = insert
   deleteSet = delete
   singletonSet (a,b) = PairSet (M.singleton a (S.singleton b))
                                (M.singleton b (S.singleton a))
   setFromList = fromList
   setToList = keys

psUnion :: (Ord a,Ord b) => PairSet a b -> PairSet a b -> PairSet a b
psUnion (PairSet la ra) (PairSet lb rb) =
      PairSet (merge la lb) (merge ra rb)
    where merge :: (Ord a, Ord b) => Map a (Set b)
                                  -> Map a (Set b)
                                  -> Map a (Set b)
          merge = M.unionWith S.union

psIntersection :: (Ord a,Ord b) => PairSet a b -> PairSet a b -> PairSet a b
psIntersection (PairSet la ra) (PairSet lb rb) =
     PairSet (go la lb) (go ra rb)
     where go :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b)
           go = M.intersectionWith S.intersection
     

psDifference :: (Ord a,Ord b) => PairSet a b -> PairSet a b -> PairSet a b
psDifference (PairSet la ra) (PairSet lb rb) =
     PairSet (go la lb) (go ra rb)
     where go :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b)
           go = M.differenceWith diff
           diff a b = let d = S.difference a b
                      in if null d then Nothing else Just d

switch :: PairSet a b -> PairSet b a
switch (PairSet a b) = PairSet b a

insert :: (Ord a,Ord b) => (a,b) -> PairSet a b -> PairSet a b
insert (a,b) (PairSet l r) = PairSet (go a b l) (go b a r)
       where  go :: (Ord a, Ord b) => a -> b -> Map a (Set b) -> Map a (Set b)
              go a b   = M.insertWith S.union a (S.singleton b)

delete :: (Ord a, Ord b) => (a,b) -> PairSet a b -> PairSet a b
delete (a,b) (PairSet l r) = PairSet (go a b l) (go b a r)
       where go a b   = M.update (del b) a
             del i s = let o = S.delete i s
                       in if null o then Nothing else Just o

lookLeft :: (Ord a,Ord b) => a -> PairSet a b -> Set b
lookLeft a (PairSet m _) = foldr const mempty $ M.lookup a m

lookRight :: (Ord a,Ord b) => b -> PairSet a b -> Set a
lookRight b m = lookLeft b (switch m)

fromList :: (Ord a, Ord b,Foldable f) => f (a,b) -> PairSet a b
fromList = foldl' (flip insert) mempty

elems :: (Ord a, Ord b) => PairSet a b -> [(a,b)]
elems (PairSet m _) = do
       (key,vals) <- M.toList m
       val <- S.toList vals
       return (key,val)

elem :: (Ord a, Ord b) => (a,b) -> PairSet a b -> Bool
elem (a,b) = (b `F.elem`) . lookLeft a

lefts :: Ord a => PairSet a b -> Set a
lefts (PairSet a _) = S.fromList $ M.keys a

leftElem :: (Ord a) => a -> PairSet a b -> Bool
leftElem a m = a `M.member` left m

rights :: Ord b => PairSet a b -> Set b
rights = lefts . switch

rightElem :: (Ord b) => b -> PairSet a b -> Bool
rightElem b m = leftElem b (switch m)

-- filter :: (Ord a, Ord b) => ((a,b)->Bool) -> PairSet a b -> PairSet a b
-- filter f (PairSet a b) = undefined

biRemove :: (Ord a, Ord b) => (a -> Bool) -> (b -> Bool) -> PairSet a b -> PairSet a b
biRemove fa fb (PairSet l r) =
  let ax = S.filter fa (M.keysSet l)
      bx = S.filter fb (M.keysSet r)
      amap = M.mapWithKey (\k v -> if fa k then v `difference` bx else v) l
      bmap = M.mapWithKey (\k v -> if fb k then v `difference` ax else v) r
  in PairSet amap bmap

