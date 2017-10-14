
{-# LANGUAGE
    MultiParamTypeClasses,
    TupleSections,
    ScopedTypeVariables,

    TypeFamilies,
    TypeApplications,
    GeneralizedNewtypeDeriving
#-}

module Main where

import NewConstraint

import Data.Proxy
import ClassyPrelude
-- TODO get rid of head
import Prelude (readsPrec,read,interact,head)
import qualified Data.Foldable as F

import Control.Monad.State.Class
import Control.Monad.State hiding (sequence_, mapM_)
import Control.Monad.Trans

import Control.Arrow hiding (first,second)
import Data.Function ((&))


main = interact (   read @Sudoku
                >>> makeGame 

                >>> makeguesses
                >>> trim row
                >>> trim column
                >>> trim box
                >>> removePlayed
                >>> execGame @[]
                >>> foldr const emptyBoard
                >>> show
                 )

-- Basic Digit Type -- represents numbers from 1 to 9
-----------------------------------------------------
-- Need To use Digits from zero to nine in two places, so make a generic type
newtype Digit = Digit Int deriving (Ord,Eq)

instance Bounded Digit where
         minBound = Digit 1
         maxBound = Digit 9

instance Show Digit where
     show (Digit i) = show i

instance Read Digit where
     readsPrec _ [] = []
     readsPrec _ (x:xs)
          | x `elem` ['1'..'9'] = [(Digit $ read [x],xs)]
          | otherwise       = []

mkDigit :: Int -> Maybe Digit
mkDigit x | x >= 1 && x <= 9 = Just $ Digit x
          | otherwise = Nothing

digits = map Digit [1..9]


-- Derived Types, representing squares on the board
---------------------------------------------------
newtype Value = Value Digit deriving (Read,Ord,Eq)
instance Show Value where show (Value x) = show x
values :: Set Value
values = setFromList $ map Value digits

newtype Square = Square (Digit,Digit) deriving (Read,Show,Ord,Eq,Bounded)
squares :: Set Square
squares = setFromList $ map Square (liftM2 (,) digits digits)

newtype Sudoku = Sudoku (Map Square Value)  deriving (Monoid)
instance Show Sudoku where
   show (Sudoku bs) = digits
           & map ((\a -> (a,)<$> digits)
               >>>concatMap (\x -> case lookup (Square x) bs of
                              Nothing -> "."
                              Just x -> show x
                            ))
           & unlines


-- Not a well behaved instance - messes with the remaining string
instance Read Sudoku where
   readsPrec _ xs = filter (`elem`('.':['1'..'9'])) xs
                  & splitAt 81
                  & first ( zip (Square <$> liftM2 (,) digits digits)
                        >>> filter ((/='.').snd)
                        >>> map (second (Value .read.return))
                        >>> mapFromList
                        >>> Sudoku)
                  & return

instance Board Sudoku where
    type Space Sudoku = Square
    type Piece Sudoku = Value
    serialise (Sudoku m) = mapToList m
    playBoard (sp,pc) (Sudoku m) = Sudoku $ insertMap sp pc m
    spaces (Sudoku m) = squares
    pieces (Sudoku m) = values
    emptyBoard = Sudoku mempty

type GSpace m = Space (GameBoard m)
type GPiece m = Piece (GameBoard m)

-- Remove Games Already Played
removePlayed :: (Game m, sq ~ GSpace m)
        => RemovePlayed sq m a -> m a
removePlayed (RemovePlayed t) = evalStateT t mempty

newtype RemovePlayed sq m a = RemovePlayed (StateT (Set sq) m a)
   deriving (Functor,Applicative,Alternative,
             Monad,MonadPlus,
             MonadTrans, MonadState (Set sq))
instance (Game m, GSpace m ~ sq, sq ~ GSpace (RemovePlayed sq m))
      => Game (RemovePlayed sq m) where
   type GameBoard (RemovePlayed sq m) = GameBoard m
   options s = do
       m <- get
       if s `elem` m then mzero else lift $ options s 
   updates [] = return ()
   updates ((sq,pc):xs) = do
       o <- options sq
       guard (pc `elem` o)
       g <- get
       guard $ not (sq `elem` g)
       modify $ insertSet sq
       s <- raise spaces
       if length g >= length s
           then return ()
           else updates xs

   open = do
       sp <- lift open
       played <- get
       return $ sp `difference` played

-- Dealing with Rows, Columns and the LIke. Make a class of VIEWS
-- =================================================================
class View a x where
         view :: a -> x

newtype Column = Column Digit deriving (Read,Show,Ord,Eq)
instance View Square Column where view (Square (x,_)) = Column x
column = Proxy :: Proxy Column

newtype Row = Row Digit deriving (Read,Show,Ord,Eq)
instance View Square Row where view (Square (_,y)) = Row y
row = Proxy :: Proxy Row

newtype Box = Box Digit deriving (Read,Show,Ord,Eq)
instance View Square Box where
   view    (Square (Digit x, Digit y)) = Box $ Digit (((x-1)`div`3)*3+((y-1)`div`3)+1)
box = Proxy :: Proxy Box

trim :: (View sq x, Game m, sq ~ GSpace m, v ~ GPiece m, Ord x)
        => Proxy x -> TrimOptions x sq v m a -> m a
trim Proxy (TrimOptions t) = evalStateT t mempty

newtype TrimOptions x sq v m a = TrimOptions (StateT (Map x (Set v)) m a)
   deriving (Functor,Applicative,Alternative,
             Monad,MonadPlus,
             MonadTrans, MonadState (Map x (Set v)))
instance (Game m, GSpace m ~ sq, GPiece m ~ v, View sq x, Ord x)
         => Game (TrimOptions x sq v m) where
    type GameBoard (TrimOptions x sq v m) = GameBoard m
    localSetup = do
        -- get the defaultSetup
        p <- raise spaces
        q <- raise pieces
        put mempty
        flip mapM_ (setToList p) $ \x -> modify (insertWith const (view x) q)

    updates xs = 
        fmap concat $ flip mapM xs $ \(sq,v) -> do
             x <- get
             let m = foldr const mempty $ lookup (view sq) x
             if  v `elem` m then do
                 modify $ insertWith (flip difference) (view sq) (setFromList [v])
             else lose 

    options x = do
          opts <- lift $ options x
          r2 <- foldr const mempty . lookup (view x) <$> get
          return $ opts `intersection` r2



newtype MultiContainer m a = MC (m a)
    deriving (Functor,Applicative,Alternative,Monad, MonadPlus)

instance MonadTrans MultiContainer where
    lift = MC 

instance Game m => Game (MultiContainer m) where
    type GameBoard (MultiContainer m) = GameBoard m
    nextplays = do
      w <- won
      if w then return []
      else do
        -- opts <- take 1 . setToList <$>  open
        opts <- setToList <$> open 
        opt2s <- mapM (\x -> map (x,) $ options x) opts
                  :: MultiContainer m [(GSpace m,Set (GPiece m))]
        let opt3s = dropWhile (null.snd) $ sortOn (length.snd) opt2s
        case takeWhile ((==1).length.snd) opt3s of
              [] -> do -- NEED TO CHECK IF WE'VE WON
                    (a,y) <- foldr const mzero $ map return opt3s
                    foldr (mplus.return) mzero $ fmap (return.(a,)) $ setToList y
                -- (a,y) <- foldr (const.return) mzero opt3s
                -- foldr (mplus.return) mzero $ fmap (\b->[(a,b)]) $ setToList y
              xs  -> return $  concatMap (\(x,y)->(x,)<$>setToList y) xs 
        -- return $  map (second $ Prelude.head . setToList)
        --        $  opt3s

makeguesses :: MultiContainer m a -> m a
makeguesses (MC m)  = m

-- newtype M m a = MCA {runM :: m a} deriving (Functor,Applicative,Alternative,Monad,MonadPlus)
-- instance MonadTrans M where lift f = MCA $ f
-- instance Game m => Game (M m) where
--   type GameBoard (M m) = GameBoard m
