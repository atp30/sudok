-- Type system extensions
{-#
LANGUAGE ExistentialQuantification,
         TypeFamilies,
         TupleSections
#-}

-- Syntax Improvements
{-#
LANGUAGE RecordWildCards
#-}

module Control.Strat where

import Prelude hiding (init)
import Control.Applicative
import Control.Arrow (first, (&&&))
import Data.Monoid
import Data.MonoTraversable
import Data.Sequences (IsSequence, singleton)

-- | Reify the applicative instance
data Appl m a = Pure (m a)
              | forall b . Appl m (b -> a) :*: Appl m b

-- | Create a strategy
data Strat q m a =  S {
        init  :: a,
        accum :: a -> q -> a,
        run   :: a -> m (q,a)
        }

upd :: q -> Strat q m a -> Strat q m a
upd q s@S{..} = s {init=accum init q}

-- | A nice wrapper for our applicative
newtype Strategy q m a = Strategy (Appl (Strat q m) a)

update :: (Monoid q) => q -> Appl (Strat q m) a -> Appl (Strat q m) a
update q (Pure s)  = Pure $ upd q s
update q (f :*: s) = update q f :*: update q s

-- | The obvious instances
instance (Monoid q, Applicative m) => Functor (Strategy q m) where
         f `fmap` s = pure f <*> s

-- | The obvious instances
instance (Monoid q, Applicative m) => Applicative (Strategy q m) where
         pure a = Strategy . Pure . S a const $ pure . (const mempty &&& id)
         Strategy mf <*> Strategy ma = Strategy $ mf :*: ma

runStrategy' :: (Monad m, Monoid q, MonoFoldable q)
    => Strategy q m a -> m (q,Strategy q m a)
runStrategy' (Strategy (Pure s@S{..})) = do
    (q,a) <- run init
    return (q,Strategy $ Pure $ s {init=accum a q})
runStrategy' (Strategy (f :*: a)) = do
    (q,Strategy f')  <- runStrategy' (Strategy f)
    if onull q
       then do (q',Strategy a') <- runStrategy' (Strategy a)
               return (q', Strategy $ update q' f' :*: a')
       else return (q, Strategy (f:*: update q a))

runStrategy :: (Monad m, Monoid q, MonoFoldable q)
    => Strategy q m a -> m (q,a)
runStrategy s = do
   (q,s') <- runStrategy' s
   if not $ onull q
      then first (q<>) <$> runStrategy s'
      else return (q,deconstruct s')
 where deconstruct :: forall q m x . Strategy q m x -> x
       deconstruct (Strategy (Pure a))  =  init a
       deconstruct (Strategy (f :*: a)) =
                   deconstruct (Strategy f) $ deconstruct (Strategy a)

strat :: a -> (a -> q -> a) -> (a -> m (q,a)) -> Strategy q m a
strat i a r = Strategy . Pure $ S i a r

makeStrategy :: (Monad m, IsSequence q, e ~ Element q)
             => a -> (a -> e -> a) -> (a -> m q) -> Strategy q m a
makeStrategy i a r = strat i (ofoldl' a) (\a -> (,a)<$>r a)

makeSink :: (MonoTraversable q, e ~ Element q, IsSequence q, Monad m)
        => (e -> m ()) -> Strategy q m q
makeSink f = strat mempty (\xs x -> xs <> x) $
         \q -> oforM_ q f >> return (mempty,mempty)

makeTactic :: Monad m => m q -> Strategy q m ()
makeTactic m = strat () const (fmap (,()) . const m)
