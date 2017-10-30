{-# LANGUAGE FlexibleContexts,
             UndecidableInstances,
             AllowAmbiguousTypes,
             FlexibleInstances,
             DeriveGeneric,

             ExistentialQuantification,
             RankNTypes,
             ScopedTypeVariables

#-}
module Control.Strategies where

import Prelude hiding (init)
import Data.Monoid
import Data.MonoTraversable
import Control.Monad
import Control.Arrow
import Test.QuickCheck
import Test.QuickCheck.Function
import GHC.Generics

import Control.Monad.Identity
data XMP q m b = Nil
               | forall a  . P (XM q m a) (a -> (b->m q)) (XMP q m b) deriving (Generic)

data XM q m a = XM {
     initial :: a,
     parent :: XMP q m a,
     accum :: a -> q -> a,
     action :: a -> m q
     } deriving (Generic)

update :: q -> XM q m a -> XM q m a
update q (XM i p acc act) =
       XM (acc i q) (updatep q p) acc act

updatep :: q -> XMP q m a -> XMP q m a
updatep q Nil = Nil
updatep q (P x m xs) = P (update q x) m $ updatep q xs

pappend :: XMP q m b -> XMP q m b -> XMP q m b
pappend Nil x = x
pappend (P a b c) x = P a b $ pappend c x

die :: (Monoid q, Monad m) => XM q m ()
die = XM () Nil const (\_->return mempty)

instance (Monad m,Monoid q) => Functor (XM q m) where
         fmap f a = a >>= return . f
instance (Monad m,Monoid q) => Applicative (XM q m) where
         pure = return
         (<*>) = ap
instance (Monad m,Monoid q) => Monad (XM q m) where
         return x = XM x Nil const (const (return mempty))
         a >>= f =
            let (XM i' u ac' m') = f (initial a)
            in XM i' (pappend u (P a (action.f) Nil)) ac' m'

runXM' :: (Monad m,MonoFoldable q,Monoid q) => XM q m a -> m (q,XM q m a)
runXM' a@(XM a0 p ac mf) = do
       q <- mf a0
       if not $ onull q
         then return (q,update q a)
         else do (q',p',mf') <- runXMparent p 
                 return (q',XM (ac a0 q') p' ac mf')
    where runXMparent (P b bf bs) = do
                (q,b') <- runXM' b
                if not $ onull q 
                  then return (q,P b' bf (updatep q bs),bf (initial b'))
                  else do
                   (q',bs',mf') <- runXMparent bs
                   return (q',P (update q' b) bf bs',mf')
          runXMparent Nil = return (mempty,Nil,const (return mempty))      


runXM :: (MonoFoldable q,Monoid q,Monad m) => XM q m a -> m (q,a)
runXM a = do
    (q,a') <- runXM' a 
    if onull q then return (mempty,initial a')
               else fmap (first (q<>)) (runXM a')
                            
              
mapper :: (Monoid q, MonoFoldable q, Monad m)
   => (q -> q) -> XM q m q
mapper f = XM mempty Nil mappend (return . f)

cluck :: Int -> XM [Int] Identity Int
cluck n = XM 0 Nil (\a q -> a + sum q) (\a -> if (a >= 0 && a<100) then return [0+n] else return [])

ticker n = XM 0 Nil (\a q -> a + length q)
       (\a -> if a > n then return [] else return [0-a])

bits :: XM [Int] Identity Bool
bits = XM False Nil (\a _ -> not a) (\a -> if a then return [99] else return [])


instance forall a b q .(Arbitrary a, CoArbitrary a, Function a,
          CoArbitrary q, Function q, Arbitrary q, Monoid q, MonoFoldable q,
          Arbitrary b, CoArbitrary b)
          => Arbitrary (XM q Identity a) where
          arbitrary = oneof 
                [ XM <$> arbitrary
                     <*> pure Nil
                     <*> (curry . apply <$> arbitrary)
                     <*> (apply <$> arbitrary)
                , liftM2 (>>=) arbitrary (arbitrary @(a -> XM q Identity b))
                ] 
          shrink = genericShrink

property_left_identity :: (Eq q, Eq b, MonoFoldable q, Monoid q)
                       => ((a -> XM q Identity b), a) -> Bool
property_left_identity (f,a) =
     runXM (return a >>= f) == runXM (f a)

property_right_identity :: (Eq q,MonoFoldable q, Monoid q,Eq a)
                      => XM q Identity a -> Bool
property_right_identity m = 
     runXM (m >>= return) == runXM m

property_associativity :: (Eq q, MonoFoldable q,Monoid q, Eq c)
  => XM q Identity a -> (a -> XM q Identity b) -> (b -> XM q Identity c) -> Bool
property_associativity m f g =
      runXM ((m >>= f) >>= g) == runXM (m >>= (\x -> f x >>= g))


