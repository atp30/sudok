
{-# LANGUAGE
    MultiParamTypeClasses,
    TupleSections,
    ScopedTypeVariables,

    TypeFamilies,
    TypeApplications,
    GeneralizedNewtypeDeriving,
    OverloadedStrings
#-}

module Main where

import Games.Sudok.Constraints
import Games.Sudok.SetCover

import Data.Proxy
import ClassyPrelude
-- TODO get rid of head
import Prelude (readsPrec,read,interact)
import qualified Data.Foldable as F

import Control.Monad.State.Class
import Control.Monad.State (StateT, evalStateT)
-- import Control.Monad.Trans

import Control.Arrow hiding (first,second)
import Data.Function ((&))

main :: IO ()
main = interact (   read @Sudoku
                >>> makeGame 
                >>> makeGuesses
                >>> exactlyOnce views
                >>> removePlayed
                >>> execGames
                >>> first (tshow . foldr const emptyBoard)
                >>> uncurry (++)
                >>> unpack
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
          | x `elem` (['1'..'9']::String) = [(Digit $ read [x],xs)]
          | otherwise       = []

mkDigit :: Int -> Maybe Digit
mkDigit x | x >= 1 && x <= 9 = Just $ Digit x
          | otherwise = Nothing

digits :: [Digit]
digits = map Digit [1..9]


-- Derived Types, representing squares on the board
---------------------------------------------------
newtype Value = Value Digit deriving (Read,Ord,Eq)
instance Show Value where show (Value x) = show x
values :: Set Value
values = setFromList $ map Value digits

newtype Square = Square (Digit,Digit) deriving (Read,Show,Ord,Eq,Bounded)
squares :: [Square]
squares = map Square (liftM2 (,) digits digits)

intercalateThrees :: a -> [a] -> [a]
intercalateThrees n (a:b:c:d:xs) = a:b:c:n:intercalateThrees n (d:xs)
intercalateThrees _ xs = xs

newtype Sudoku = Sudoku (Map Square Value)  deriving (Monoid)
instance Show Sudoku where
   show (Sudoku bs) = digits
           & map ((\a -> (a,)<$> digits)
               >>>concatMap (\x -> case lookup (Square x) bs of
                                    Nothing -> "."
                                    Just y -> show y
                            )
               >>>intercalateThrees ' ')
           & intercalateThrees ""
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
    boardMoves _ = mapFromList $ zip squares (repeat values)
    emptyBoard = Sudoku mempty


data View = Box (Int,Int) | Row Digit | Column Digit deriving (Eq,Ord,Show)

box, row, column :: Square -> View
box     (Square (Digit x, Digit y)) = Box ((x-1)`div`3,(y-1)`div`3)
row     (Square (x,_))              = Row x
column  (Square (_,y))              = Column y

views :: Square -> [View]
views x = [box x,row x,column x]


