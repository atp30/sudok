{-# LANGUAGE
    NoImplicitPrelude,
    MultiParamTypeClasses,
    ScopedTypeVariables,
    TypeFamilies,
    ConstraintKinds,
    GeneralizedNewtypeDeriving,
    ExplicitForAll,
    TupleSections,
    OverloadedStrings

#-}

module Games.Sudok.Constraints where

import Data.PairSet
import ClassyPrelude hiding (log,Map)
import Data.Map.Strict (Map)
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import ListT (ListT)
import qualified ListT as LT
-- import Control.Monad.List

import qualified Data.Text as T
import Data.Function ((&))

-- Games
--------

newtype Game s p a = G (StateT (PairSet s p)
                               (ListT
                               (Writer (CountSet Text)
                               )) a)
        deriving (Monad,Functor,Applicative,MonadState (PairSet s p))

log :: Text -> Game s p () 
log = G . lift . lift . tell . csSingleton

won :: (Ord s, Ord p) => Game s p Bool
won = G $ get >>= return . null 

newtype CountSet a = CountSet (Map a Int) deriving (Read)

csToList :: Ord a => CountSet a -> [(a,Int)]
csToList (CountSet a) = mapToList a

csSingleton :: Ord a => a -> CountSet a
csSingleton a = CountSet $ singletonMap a 1

instance Ord a => Monoid (CountSet a) where
   mempty = CountSet mempty
   mappend (CountSet a) (CountSet b) =
      CountSet $ unionWith (+) a b

pad :: Int -> String -> String 
pad n t = t ++ (replicate (n-length t) ' ')

instance (MonoFoldable a, Element a ~ Char, Ord a) => Show (CountSet a) where 
   show (CountSet a) = a
     & mapToList
     & sortOn snd
     & reverse
     & map (\(a,b) -> pad 30 (unpack a++":")++" "++show b)
     & unlines
