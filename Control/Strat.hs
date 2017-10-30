-- Type system extensions
{-#
LANGUAGE ExistentialQuantification
#-}

-- Syntax Improvements
{-#
LANGUAGE RecordWildCards
#-}

module Control.Strat where

import Prelude hiding (init)
import Control.Applicative
import Control.Arrow (first)
import Data.Monoid
import Data.MonoTraversable

-- | Reify the applicative instance
data Appl m a = Pure (m a)
              | forall b . Appl m (b -> a) :*: Appl m b

-- | Create a strategy
data Strat q m a =  S {
        init  :: a,
        accum :: a -> q -> a,
        run   :: a -> m q
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
         pure a = Strategy $
              Pure (S a const (const $ pure mempty))
         Strategy mf <*> Strategy ma = Strategy $
              mf :*: ma


runStrategy' :: (Monad m, Monoid q, MonoFoldable q)
    => Strategy q m a -> m (q,Strategy q m a)
runStrategy' (Strategy s@(Pure S{..})) = do
    q <- run init
    return (q,Strategy $ update q s)
runStrategy' (Strategy (f :*: a)) = do
    (q,Strategy f')  <- runStrategy' (Strategy f)
    if onull q
       then do (q',Strategy a') <- runStrategy' (Strategy a)
               return (q', Strategy $ update q' f' :*: a')
       else return (q,Strategy $ f' :*: update q a)

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


makeStrategy :: Strat q m a -> Strategy q m a
makeStrategy s = Strategy (Pure s)
