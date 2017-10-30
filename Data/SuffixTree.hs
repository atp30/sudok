{-#
LANGUAGE TypeFamilies,
         ScopedTypeVariables,
         TypeApplications
#-}

module Data.SuffixTree where

import Prelude hiding (lookup)
import Data.Sequences
import Data.Monoid
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.MonoTraversable
import Control.Arrow (( &&& ))
import Data.Function ((&))

-- Indexed from the right - sequences carry their length for efficiency
data SuffixTree s e = Sequence !s !(SuffixTree s e)
                    | Division !(Map e (SuffixTree s e))
                    | End !Int
     deriving (Read,Show,Eq)

appseq :: (MonoFoldable s, e ~ Element s, Monoid s) => s -> SuffixTree s e -> SuffixTree s e
appseq s (Sequence s' rest) =
       Sequence (s<>s') rest
appseq s n | onull s = n
           | otherwise = Sequence s n

merge :: (Ord a, IsSequence s, a ~ Element s)
      => SuffixTree s a -> SuffixTree s a -> SuffixTree s a
--THIS ISN@T ALLOWED !!
a `merge` (End _) = a
(Division m) `merge` (Division n) =
          Division $ M.unionWith (merge) m n
(Division m) `merge` (Sequence s n) = Division $
          case (uncons s) of
               Nothing ->  m
               Just (a,ax) -> M.insertWith merge a (appseq ax n) m
(Sequence as an) `merge` (Sequence bs bn) =
          case (uncons as,uncons bs) of
               (Just (a',as'),Just (b',bs')) -> if a' == b'
                    then appseq (singleton a') $ appseq as' an `merge` appseq bs' bn
                    else Division  (M.fromList [(a',appseq as' an),
                                                (b',appseq bs' bn)])
               (a,Nothing) -> Sequence as an `merge` bn
               (Nothing,b) -> an `merge` Sequence bs bn
a `merge` b = b `merge` a

makeTree :: ( MonoFoldable s, IsSequence s, a ~ Element s, Ord a )
         => s -> SuffixTree s a
makeTree s = tails s
           & zipWith (\i s -> Sequence s (End i)) [0..]
           & ofoldl' merge (End (-1))
    

tails xs = case uncons xs of
    Nothing -> []
    Just (x,xs) -> cons x xs : tails xs

indices :: Ord e => SuffixTree s e -> Set Int
indices (Sequence _ n) = indices n
indices (Division m) = foldMap indices m
indices (End i) = S.singleton i

lookup :: (IsSequence s, Ord e, e ~ Element s) => s -> SuffixTree s e -> Set Int
lookup s (End i) | onull s = S.singleton i
                 | otherwise = mempty
lookup s (Division m) = case uncons s of
       Nothing -> indices (Division m)
       (Just (a,b)) -> case M.lookup a m of
             Nothing -> mempty
             Just c -> lookup b c
lookup s (Sequence sm n) = case (uncons s,uncons sm) of
       (Nothing,_) -> indices n
       (_,Nothing) -> lookup s n
       (Just (s',ss'),Just (sm',sms')) ->
          if s' == sm' then lookup ss' $ appseq sms' n
                       else mempty

