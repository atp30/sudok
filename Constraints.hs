{-# LANGUAGE NoImplicitPrelude,
             MultiParamTypeClasses,
             RankNTypes,
             NoMonomorphismRestriction
#-}

module Constraints where

import ClassyPrelude
import qualified Data.Foldable as F
import Control.Lens
import Control.Arrow ((***),(>>>))
import Control.Applicative
import Data.Function ((&))
import Data.Traversable



-- A Traversable Version of Zip
-- From SO:
-- https://stackoverflow.com/questions/41522422

tzipWith :: (Foldable f, Traversable t) => (a->b->c) -> f a -> t b -> t c
tzipWith f xs ys = val
    where Just val = sequenceA . snd . mapAccumL pair (F.toList xs) $ ys
          pair [] _ = ([],Nothing)
          pair (x:xs) y = (xs, Just $ f x y)


-- We can compose in the views (traversals/...)
--      to include a setter of the form a -> New Set
--      getter of type Old Set
-- What we cannot do

-- Some Sort of Monad Transformer Stack
-- At it's heart, there is a set action
-- Setting at any level sets at all levels
-- Each carries it's own additional context
-- can ignore the setting

-- Need to be able to build the context into the traversal

-- I want to be able to pass a set of lenses over

-- should I be composing lenses directly? If so, how?

-- We want to extract a value and one level of context

-- I want to do two things
-- List appropriate actions

annotated :: Lens' b c
          -> Lens' s z
          -> (z -> z -> c -> c)
          -> Lens' (b,s)  z
annotated annotatelens mainlens updater = lens viewer setter
     where viewer (_,s) = view mainlens s
           setter (b,s) x =
              let xold = view mainlens s
              in (b & over annotatelens (updater xold x)
                 ,s & set mainlens x )
                  

reannotated :: (forall a. Lens' (s a) a)
            -> (z -> z -> c -> c)
            -> Lens' (s c,s z) z
reannotated l = annotated l l


class (Foldable s, Traversable s) => Game s a where
      won :: Foldable s => s (Maybe a) -> Maybe (s a)
      won = sequenceA  

      forbidden :: s (Maybe a) -> Bool
      forbidden _ = False

      traverser :: Traversal (s (Maybe a)) (s (Maybe a)) (Maybe a) (Maybe a)
      traverser = traversed

      

play :: Game s a => (Maybe a->Maybe a) ->  s (Maybe a) -> s (Maybe a)
play = over traverser 
--
--    Cannot currently think of a sensible default
--
--    empties :: Traversal (s (Either u a)) (s (Either u a)) u (Maybe a)
--    empties = traversed . filtered isLeft . lens uneither reeither 
--        where uneither (Right _) = error "impossible (Constraint.empties)"
--              uneither (Left  x) = x
--              reeither x Nothing = x
--              reeither _ (Just y) = y

data Hueristic c s a =
     Heuristic {
        
        ame :: s (Maybe a)
     }


-- What we need is:
--  A transformer that completely takes over the underlying monad
--  A run rule -- which writes back to parent monads at every step
--  A failure exit condition
--  A success exit condition

-- want stateT - state needs to remain in place, m


type Player b m x = StateT (Maybe b) m (Play x) 

data Play i v = Play {index :: i, value :: v}


class MonadPlayer m where
   won   :: m a -> Bool
   -- Indicate a play, get back a load of plays
   plays :: Play i v -> m [Play i v]

whileM :: (a -> Bool) -> m a -> m [a]
whileM p m = do
     x <- m
     if p x then (x:)<$>whileM p m
            else return []

instance (MonadPlayer m, Wrapper p) => MonadPlayer p m where
   plays = return . fmap concatMap . whileM (not.null)

wrap :: (Playable a, Player m) => m a -> m (f a)

class Movable board s

class Player piece board s where  
   play :: 

-- Ga
