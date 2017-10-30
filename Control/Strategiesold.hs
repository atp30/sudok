{-# LANGUAGE FlexibleContexts,
             UndecidableInstances,
             FlexibleInstances,

             ExistentialQuantification,
             RankNTypes

#-}
module Control.Strategies where

import Prelude hiding (init)
import Data.DList hiding (foldr,Nil)
import Data.Monoid
import Data.MonoTraversable
import Control.Monad
import Control.Arrow
import Test.QuickCheck

import Control.Monad.Identity
-- data X q m a = X {init :: a,
--                   acc  :: a -> (DList q) -> a,
--                   next :: a -> m (DList q,X q m a)
--                   }



-- runX :: Monad m => X q m a -> m (DList q,a)
-- runX x0@(X a0 aAcc aNext) = do
--      (q,next) <- aNext a0
--      if null q then return (mempty,a0)
--              else fmap (first (q<>)) $ runX $ next {init = aAcc a0 q}
--      -- if null q then return (mempty,a0)
--      --           else fmap (first (q<>)) $ runX $ xnext{init=aAcc a0 q}

-- push :: Monad m => q -> X q m ()
-- push n = X () const (\_ -> return (singleton n,push n))

-- pushonly :: Monad m => q -> X q m (Maybe q) 
-- pushonly n = X (Just n) (const $ const Nothing)
--                (\a -> return (foldr (const.pure) mempty a,pushonly n))

-- die :: Monad m => X q m ()
-- die = X () const (\_ -> return (mempty,die))

-- instance Monad m => Functor (X q m) where
--          fmap f a = a >>= return . f
-- instance Monad m => Applicative (X q m) where
--          pure = return
--          (<*>) = ap
-- instance Monad m => Monad (X q m) where
--          return a = X a const (\b -> return (mempty,return a))
--          am@(X a0 aAcc aNext) >>= f =
--            let bm@(X b0 bAcc bNext) = f a0
--            in X b0 bAcc $ \b -> do
--                (q,bm') <- bNext b -- on failure, throw away monad
--                if null q
--                   then
--                    do (q',am') <- aNext a0
--                       return (q',(am'{init=aAcc a0 q'}>>=f){init=bAcc b' q'})
--                   else return (q'<>q'',
--                        (am{init=aAcc a' q}>>=f){init=bAcc b' q})
-- xrepeat :: Monad m => Int -> (q -> q -> q) -> q -> X q m q 
-- xrepeat n f q0 = X q0 (\q qs -> foldl' f q qs) (\q' -> return $ 
--               (pure q',if n>1 then xrepeat (n-1) f q' else return q'))
           

data XMP q m b = Nil
               | forall a  . P (XM q m a) (a -> (b->m q)) (XMP q m b)


data XM q m a = XM {
     initial :: a,
     parent :: XMP q m a,
     accum :: a -> q -> a,
     action :: a -> m q
     }

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


-- (X f0 fqf fm)(<*>)(X a0 aqf am) =
--    X (f0 a0) (\q -> (fqf q) (aqf q)) $ do
--       (q',am') <- am

            -- wX b0 bAcc $ \b -> do
            --     (q,bm') <- bNext b
            --     if null q
            --       then do (q',am') <- aNext a0
            --               return (q',(am'{init=aAcc a0 q'}>>=f){
            --                         init = bAcc b q'
            --                         })
            --       else (q''<-)
                       

-- return (q,(am{init = aAcc a0 q}>>=f){
--                                       init = bAcc b q})


-- newtype SM q m a =
--     SM (Maybe a -> (DList q) ->  (a,m (DList q)))

-- whileM :: Monad m => (a -> Bool) -> a -> (a -> m a) -> m a
-- whileM p v st  =  
--      do v2 <- st v
--         if p v2
--           then whileM p v2 st
--           else return v2



-- runSM :: Monad m => SM q m a -> DList q -> m a
-- runSM (SM f) qs = do
--     let (a,m) = f Nothing qs
--     q' <- m
--     let a' = fst (f (Just a) q')
--     return a'

-- execSM :: Monad m => SM q m a -> DList q -> (a,DList q)
-- execSM (SM f) qs = do
--        let (a,m) = f Nothing qs
       

-- instance Monad m => Functor (SM q m) where
--          fmap f a = a >>= return . f
-- instance Monad m => Applicative (SM q m) where
--          pure = return
--          (<*>) = ap
-- instance Monad m => Monad ( SM q m ) where
--     return x = SM (\_ _ -> (x, return mempty))
--     (SM faq) >>= f =
--        SM $ \b q ->
--         let (a',_) = faq Nothing q -- make sure we're up to date, but don't run
--             SM fb = f a'
--         in  (fst $ fb b q,
--               do   (b'',qs',q') <- whileM
--                           (\(_,_,qq)->not $ null qq)
--                           (b,mempty,q) $ \(_b,_qs,_q) -> do
--                                   let (_b',mb) = fb _b _q 
--                                   _q' <- mb
--                                   return (Just _b',_qs<>_q,_q')
--                    q'' <- am
--                    return (qs'<>q'<>q'')
--                 )
         

-- push :: Monad m => q -> SM q m ()
-- push q = SM $ \_ _ -> ((),return $ return q)

-- pull :: Monad m => DList q -> SM q m (DList q)
-- pull q = SM $ \_ q -> (q,return mempty)

-- -- acc :: Monad m => SM q m (DList q)
-- -- acc = SM $ \qs q -> (foldr const mempty qs<>q,return mempty)

-- -- data XS q m a = XS {acc::Maybe a -> DList q -> a,
-- --                     act::a -> m (a,DList q,XS q m a) }
-- -- instance Monad m => Functor (XS q m) where
-- --          fmap f a = a >>= return . f
-- -- instance Monad m => Applicatiev (XS q m) where
-- --          pure = return
-- --          (<*>) = ap
-- -- instance Monad m => Monad (XS q m) where
-- --          return x = XS (const (const x),const (return mempty))

-- --          a >>= f = abind Nothing a f

-- -- abind :: Maybe a -> q -> XS q m a -> (a -> XS q m b) -> XS q m b
-- -- abind a0 q0 XS{aacc=acc,aact=act} f = 
-- --       let a = aacc a0 q0
-- --           bm@XS{bacc=acc,bact=act} = f a --note, bacc is forbidden from dependin on a
-- --       in XS bacc $ \b -> do
-- --             (b',bm',qs',q',a') <- whileM
-- --                 (not.null.(\(_,_,qq)->qq))
-- --                 (b,bm,mempty,q,a) $ \(_b,_,_qs,_q,_a) -> do
-- --                       let _b' = bacc _b _q
-- --                       (_q',_bm') <- _b' & act (f _a)
-- --                       let _a' = aacc _a _q'
-- --                       return (Just _b',_bm',_qs<>_q,_q',_a')

-- -- abind a0 q0 XS{aacc=acc,aact=act} f =
-- --        let a = aacc a0 q0
-- --             XS{bacc=acc,bact=act} = f a
-- --        in XS bacc $ \b -> do
-- --            let b' = bacc b q0
-- --            (q',bm') = bact b'
-- --            if null q' then do
-- --               (q'',am') <- aact a
-- --               return (q'',abind (Just a) q'' (XS aacc am') $
-- --                              \a -> XS bacc)
              

-- ATTEMPTS AT QUICKCHECK INSTANCES
-- SEEMS TO BE FALSIFIABLE
-- instance (Monad m, Show q, Show a, Show (m (DList q, X q m a))) =>
--      Show (X q m a) where
--      show (X i a n) = "[:" ++ show i ++ " : " ++ show (n i) ++ ":]"

-- instance (Monad m, Show q, Show a, Show (m (DList q, X q m a))) =>
--      Show (a -> X q m a) where
--      show m = "(-FUNC-)"

-- instance (Arbitrary a) => Arbitrary (DList a) where
--       arbitrary = fmap fromList arbitrary

-- instance CoArbitrary a => CoArbitrary (DList a) where
--       coarbitrary = coarbitrary . toList

-- instance (Arbitrary a,         CoArbitrary a,         
--           Arbitrary q, CoArbitrary q, 
--           Monad m, Arbitrary (m (DList q, X q m a)))            
--        => Arbitrary (X q m a) where
--        arbitrary = liftM3 X arbitrary arbitrary (return <$> arbitrary)

-- property_left_identity :: (Eq (m (DList q,b)),Show (m(DList q,b)), Monad m)
--                        => ((a -> X q m b), a) -> Bool
-- property_left_identity (f,a) =
--      runX (return a >>= f) == runX (f a)

-- property_right_identity :: (Eq (m (DList q,a)),Show (m(DList q,a)), Monad m)
--                         => X q m a -> Bool
-- property_right_identity m = 
--      runX (m >>= return) == runX m

-- property_associativity :: (Eq (m (DList q,c)), Monad m)
--   => X q m a -> (a -> X q m b) -> (b -> X q m c) -> Bool
-- property_associativity m f g =
--       runX ((m >>= f) >>= g) == runX (m >>= (\x -> f x >>= g))

---
