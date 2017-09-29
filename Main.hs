-- A Sudoku Solver

-- An excercise
--      solve a problem I haven't tried before
--      become more comfortable with lenses

-- Reads a game from stdin
-- format : directly encode with . for blanks
-- >...26.7.1
-- >68..7..9.
-- >19...45..
-- >82.1...4.
-- >..46.29..
-- >.5...3.28
-- >..93...74
-- >.4..5..36
-- >7.3.18...

{-# LANGUAGE
    NoImplicitPrelude,
    TemplateHaskell #-}
module Main where

import ClassyPrelude
import Data.Array
import Data.Lens
import Data.Array.Lens
import Data.Function (&)

main :: IO ()
main = putStrLn "Hello, Haskell!"


-- we need to be able to index an arbitrary column by
-- other elements in the same row, column, and box
type Value = Char

newtype Remaining = Remaining (Array Int (Set Value))

data Index = Index { _row::Int, _col::Int, _box::Int}
makeLenses ''Index

data BoardLookups = BL {
        _rows :: Remaining,
        _cols :: Remaining,
        _boxes :: Remaining
     }
makeLenses ''BoardLookups

data Board = Board {
     _values :: Array Index (Maybe Value),
     _lookups :: BoardLookups
     }
makeLenses ''Board

makeIndex :: Int -> Int  -> Index
makeIndex a b
    | a < 1 || a > 9 = error "bad index"
    | b < 1 || b > 9 = error "bad index"
    | otherwise      = Index a b $ (a`div`3)*3+(b`div`3)


possibles :: Index -> Board -> Maybe (Set Value)
possibles index board = 
    case (board ^. values . ix index) of
         Just _ -> Nothing
         Nothing -> Just $
            (board ^. lookups . rows . ix (index ^. row))
              `itersection`
            (board ^. lookups .cols . ix (index ^. col))
              `itersection`
            (board ^. lookups . boxes . ix (index ^. box))

play :: Index -> Value -> Board -> Board
play index value board = board
     & set (values.ix index._Just) value
     & over (rows.ix (index^.row)) (delete value)
     & over (cols.ix (index^.col)) (delete value)
     & over (boxes.ix (index^.box)) (delete value)


