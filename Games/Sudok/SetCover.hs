{-#
LANGUAGE
        NoImplicitPrelude,
        GeneralizedNewtypeDeriving,
        TypeFamilies,
        TupleSections,
        TypeApplications,
        ScopedTypeVariables,
        GADTs,
        OverloadedStrings
#-}

module Games.Sudok.SetCover where

import Games.Sudok.Constraints
import ClassyPrelude hiding (log)

import qualified Data.Foldable as F
import qualified Data.Map as M

import Control.Monad.State (StateT, MonadState, get, put, modify, evalStateT)
import Data.Function ((&))
import Control.Arrow ((>>>))


fhead :: (MonoFoldable fa, a ~ Element fa, Monoid a) => fa -> a
fhead = foldr const mempty

mapFromFoldableWith :: (Foldable f, Ord b) => (a -> a -> a) -> f (b,a) -> Map b a
mapFromFoldableWith f = F.foldl' (flip . uncurry $ insertWith f) mempty 

-- 1. Remove Options after a piece has been played
--------------------------------------------------

removePlayed :: (Game m, sq~GSpace m, v ~ GPiece m) =>
             RemovePlayed sq v m a -> m a
removePlayed (RemovePlayed x) = evalStateT x mempty

newtype RemovePlayed sq v m a = RemovePlayed (StateT (Map sq (Set v)) m a)
   deriving (Functor,Applicative,Alternative,
             Monad,MonadPlus,
             MonadTrans, MonadState (Map sq (Set v)))

instance (Game m, GSpace m ~ sq, sq ~ GSpace (RemovePlayed sq v m),
                  GPiece m ~ v, v ~ GPiece (RemovePlayed sq v m), Show sq)
      => Game (RemovePlayed sq v m) where
   type GameBoard (RemovePlayed sq v m) = GameBoard m

   localSetup = raise boardMoves >>= put
   options = get
   updates = mapM_ $ \(sq,_) -> do
                g <- get
                guard (sq `member` g) 
                modify $ deleteMap sq

-- 2. Prevent multiple 

newtype ForbidTwice x sq v m a = ForbidTwice (
                ReaderT (sq -> [x]) (
                  StateT (Map x (Set v)) m) a)
   deriving (Functor,Applicative,Alternative,Monad,MonadPlus)

instance MonadTrans (ForbidTwice x sq v) where
  lift = ForbidTwice . lift . lift

forbidTwice :: (Game m, sq~GSpace m, v ~ GPiece m, Ord x) =>
            (sq -> [x]) -> ForbidTwice x sq v m a -> m a
forbidTwice f (ForbidTwice x) = evalStateT (runReaderT x f) mempty

instance (Game m, GSpace m ~ sq, GPiece m ~ v, Ord x, Show x, Show sq, Show v) =>
         Game (ForbidTwice x sq v m) where
   type GameBoard (ForbidTwice x sq v m) = GameBoard m
   playName = return "Prevent from occuring twice"
   localSetup = do
       ForbidTwice $ put mempty
       f   <- ForbidTwice ask
       mvs <- mapToList <$> raise boardMoves
       let pairs = do
              (key,val) <- mvs
              views <- f key
              return (views,val)
       ForbidTwice $ put mempty
       forM_ pairs $ \(k,v) ->
          ForbidTwice $ modify (insertWith union k v)

   updates = mapM_ $ \(sq,v) -> 
      do view <- ForbidTwice ask
         forM_ (view sq) $ \s -> do
            x <- fhead . lookup s <$> ForbidTwice get
            guard (v `elem` x)
            ForbidTwice . modify $ adjustMap (deleteSet v) s

   nextplays =
       do o  <- options
          return $ o
                 & mapToList 
                 & filter ((==1).length.snd)
                 & concatMap (\(a,b)-> (a,)<$>foldr (const.return) [] b)

   options = do
       f <- ForbidTwice ask
       q <- ForbidTwice get
       m <- lift options
       return $ flip M.mapWithKey m $
              \k v -> foldl' intersection v (fhead . flip lookup q <$> f k)


-- Require each element to appear once per view 
----------------------------------------------

requireEach :: (Game m, sq~GSpace m, v ~ GPiece m, Ord x) =>
            (sq -> [x]) -> RequireEach sq v x m a -> m a
requireEach f (RequireEach x) = runReaderT x f

newtype RequireEach sq v x m a = RequireEach (ReaderT (sq -> [x]) m a)
   deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance MonadTrans (RequireEach sq v x)
    where lift = RequireEach . lift

instance (Ord x, Game m, sq~ GSpace m, Show sq, Show v, Show x, v ~ GPiece m)
     => Game (RequireEach sq v x m) where
    type GameBoard (RequireEach sq v x m) = GameBoard m
    playName = return "Require to occur once"
    nextplays = do
        view <- RequireEach ask
        o <- mapToList <$> options
        let pairs = do
              (sq,pieces) <- o
              piece <- setToList pieces
              x <- view sq
              return (x,[(piece,sq)])

            bigmap = mapFromFoldableWith (++) pairs
                   & map (map (second (return @[]))
                      >>> mapFromFoldableWith (++)
                      >>> M.filter ((==1).(length @[_]))
                      )
                   & M.filter (not.null)
                   & M.elems
                   & concatMap (mapToList
                       >>> concatMap (\(a,bs)->(,a)<$>bs)
                       )
                   -- This is important, otherwise we try to play duplicates!
                   & setToList @(Set _) . setFromList

        return bigmap

exactlyOnce :: (Ord x, Game m, sq ~ GSpace m, v ~ GPiece m, Show sq, Show x, Show v)
            => (sq -> [x]) -> RequireEach sq v x (ForbidTwice x sq v m) a -> m a
exactlyOnce f = requireEach f >>> forbidTwice f

-- Make Guesses
---------------

newtype Guess sq v m a = Guess (m a)
    deriving (Functor,Applicative,Alternative,Monad, MonadPlus)

instance MonadTrans (Guess sq v) where
    lift = Guess 

instance (Game m, sq ~ GSpace m, v ~ GPiece m, Show sq, Show v)
       => Game (Guess sq v m) where
    type GameBoard (Guess sq v m) = GameBoard m
    playName = return "Guess when stuck"
    nextplays = do
        w <- won
        if w then return []
        else do
          -- opts <- take 1 . setToList <$>  open
          opts <- mapToList <$> options
          let opt3s = dropWhile (null.snd) $ sortOn (length.snd) opts
          case takeWhile ((==1).length.snd) opt3s of
                [] -> do -- NEED TO CHECK IF WE'VE WON
                      (a,y) <- foldr (const.return) mzero opt3s
                      foldr (mplus.return) mzero $ (return.(a,)) <$> setToList y
                xs  -> do
                    return $  concatMap (\(x,y)->(x,)<$>setToList y) xs 

makeGuesses :: Guess sq v m a -> m a
makeGuesses (Guess m)  = m
