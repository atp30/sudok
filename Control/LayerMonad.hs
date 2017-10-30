{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.LayerMonad where

import Prelude
import Data.Monoid ((<>))
import Data.DList (DList,singleton,fromList,toList)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad
import Control.Arrow

-- -- -- AppList - A version of DList with O(1) length and no subtractive methods
-- -- data AppList a = AL !Int (DList a)
-- -- instance Monoid (AppList a) where
-- --      mempty = AL 0 mempty
-- --      mappend (AL xn xs) (AL yn ys) = AL (xn+yn) (xs `mappend` ys)
-- -- instance Foldable (AppList a) where
-- --      foldMap f (AL _ x) = foldMap f x
-- --      foldr f z (AL _ x) = foldr f z x
-- --      length (AL n _) = n
-- -- instance Functor (AppList a) where
-- --      fmap f (AL n x) = AL n (fmap f x)
-- -- instance Applicatve (AppList a) where
-- --      pure a = AL 1 (pure a)
-- --      (AL nx xs)<*>(AL ny ys) = AL (nx*ny) (xs <*> ys)

-- -- RWT represents read-or-append state.
-- When run, only returns the appended portion
 newtype RWT rs m a = RWT (
                              ReaderT (DList rs) (
                              StateT (DList rs) m) a)
    deriving (Functor,Applicative,Monad)

instance MonadTrans (RWT rs) where
     lift = RWT . lift . lift

pull :: Monad m => RWT rs m (DList rs)
pull = RWT $ liftM2 (<>) ask get

push :: Monad m => rs -> RWT rs m ()
push a = RWT $ modify (<>return a)

pushes :: Monad m => DList rs -> RWT rs m ()
pushes a = RWT $ modify (<> a)

runRWT :: RWT rs m a -> (DList rs) -> m (a,DList rs)
runRWT (RWT m) rin = runStateT (runReaderT m rin) mempty


-- newtype LayerMonad q m a = LayerMonad (RWT q m (Maybe a) ->
--                                        RWT q m (Either (LayerMonad q m a) a))

-- runLayerMonad :: Monad m => (LayerMonad q m a) -> DList q ->  m (a,DList q)
-- runLayerMonad (LayerMonad m) q = do
          
--           let m1 = m Nothing
--           (a,q2) <- runRWT m1 q
--           return (a, q2)


-- -- I need some sort of ReaderState, which:
-- --      Allows me to read everything I've put so far
-- --      Put in new stuff
-- --      Extract the difference between what was put in at the top, and what wasn't

-- instance Monad m => Functor (LayerMonad q m) where
--          fmap f a = a >>= return . f

-- instance Monad m => Applicative (LayerMonad q m) where
--          pure = return
--          (<*>) = ap

-- instance Monad m => Monad (LayerMonad q m) where
--          return = LayerMonad . const . return
--          (LayerMonad m) >>= f = abind Nothing m f

-- -- HOW DO WE MAKE THIS ASSOCIATIVE
-- abind :: Monad m => Maybe a
--                 -> LayerMonad q m a
--                 -> (a -> LayerMonad q m b)
--                 -> LayerMonad q m b
-- abind ma (lm@(LayerMonad m)) f = LayerMonad $ \b ->
--          do a <- ma
--             a' <- m a
--             q <- pull
--             let LayerMonad fa = f a'
--             (b', q') <- lift $ runRWT (fa b) q
--             if null q'
--                 then return b' -- q == qnext, as append only, so no need to put
--                 else do
--                   pushes q'
--                   let LayerMonad mb' = abind (Just a') lm f
--                   (b'',q'') <- lift $ runRWT (mb' . return $ Just b') q'
--                   pushes q''
--                   return b''
                              
                              

-- record :: Monad m => q -> LayerMonad q m ()
-- record x = LayerMonad $ pure $ push x

-- collect :: Monad m => LayerMonad q m [q]
-- collect = LayerMonad $ pure $ toList <$> pull

-- ---
newtype StateM m a = SM (Maybe a ->m (a,StateM m a))
runSM (SM a) = a
newtype StateS s a = SA {runSS::(s->(a,s))}
instance Functor (StateS s) where
     fmap f (SA fs) = SA $ first f . fs 
instance Applicative (StateS s) where
     pure = return
     (<*>) = ap
instance Monad (StateS s) where
     return a = SA $ \s -> (a,s)
     (SA fm) >>= f  = SA $ fnew
        where fnew s = let (a,s') = fm s
                           SA f'  = f a
                       in  f' s'
instance Monad m => Functor (StateM m) where
     fmap f m = m >>= return . f
instance Monad m => Applicative (StateM m) where
     pure = return
     (<*>) = ap
instance Monad m => Monad (StateM m) where
     return a = SM $ \m -> return (a,return a)
     (>>=) = mabind Nothing



mabind :: Monad m => (Maybe a) -> StateM m a -> (a -> StateM m b) -> StateM m b
mabind ma (SM fm) f = SM $ \b -> do
  (a',m') <- fm ma -- run the current incantation?
  if not (null ma) -- pass on to the next layer
    then do let SM fn = (f a')
            (b', mb) <- fn b
            return $ (b', mabind (Just a') m' (const mb))
    else runSM (mabind (Just a') m' f) b -- This needs to go further back!
            
instance MonadTrans (StateM) where
  lift m = SM $ const (fmap (,lift m) m)
                        
-- newtype X m a = X {rX :: Maybe a -> (q -> a,RWT q m (a,X m a -> X m a))
-- instance Monad m => Functor (X m) where
--  fmap f m = m >>= return . f
-- instance Monad m => Applicative (X m) where
--          pure = return
--          (<*>) = ap
-- instance Monad m => Monad (X m) where
--          return x = X (const x, return (x,id))
--          (>>=) = xbind Nothing

-- xbind :: Monad m => Maybe a -> X m a -> (a -> X m b) -> X m b
-- xbind a q (X fa) f = let (faq,ma) = fa a
--                          a' = faq q
--                          X bm = f a'
--   in X $ \b -> do
--       let (fbq,bm') = bm b 
--           b' = fbq b
--       ((newb,newbm), q') <- lift $ runRWT (bm'(X bm)) q -- same updates get passed down
--       pushes q'
--       ((newa,newam),q'') <- lift $ runRWT (ma (X fa)) q'
--       if null q''
--          then return (newb,newbm bm) -- discard the new a results as invalid
--          else xbind (Just newa) newam
      

     

                        

data X m a = X {m (Maybe a, Maybe a -> ((DList q) -> a,m (DList q,X m a)))}
instance Monad m => Functor (X m) where
 fmap f m = m >>= return . f
instance Monad m => Applicative (X m) where
         pure = return
         (<*>) = ap
instance Monad m => Monad (X m) where
         return x = X (const x, return mempty)
         (>>=) = xbind mempty

xbind :: Monad m => Maybe a -> (DList q) -> X m a -> (a -> X m b) -> X m b
xbind q (X a fa) f = do (faq,ma) <- fa a
                         let a' = faq q
                             X bm = f a'
                            (fbq,mb) = fb b 
                         let b' = fbq q
                    in X b' $ do
                         (q',mb') <- mb -- same updates get passed down
                         if null q'
                            -- no new changes for b state
                            -- 
      
