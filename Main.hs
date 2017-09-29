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
    TupleSections,
    TypeFamilies,

    TemplateHaskell,

    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable

#-}

module Main where

import ClassyPrelude
import Data.Array
import Data.Traversable (mapAccumL)
import Control.Lens
import Data.Array.Lens
import Data.Function ((&))
import Data.Text.IO (interact)
import qualified Data.Foldable as F



-- A Traversable Version of Zip
-- From SO:
-- https://stackoverflow.com/questions/41522422/whats-the-most-standard-generic-way-to-zip-a-traversable-with-a-list
tzipWith f xs ys = val
    where Just val = sequenceA . snd . mapAccumL pair (F.toList xs) $ ys
          pair [] y = ([],Nothing)
          pair (x:xs) y = (xs, Just $ f x y)

initSet :: Set Value
initSet = setFromList ['1'..'9']


-- Basic Container Types
type Value = Char
type Remaining = (Array Int (Set Value))

-- BoardIndex Types - allow us to map over rows, columns and boxes
-- we need to be able to BoardIndex an arbitrary column by
-- other elements in the same row, column, and box
data GenBoardIndex a = BoardIndex { _row::a, _col::a, _box::a}
     deriving (Functor,Foldable,Traversable,Show)

type instance Element (GenBoardIndex a) = a

instance MonoFunctor (GenBoardIndex a)
instance MonoFoldable (GenBoardIndex a)
instance MonoTraversable (GenBoardIndex a)
makeLenses ''GenBoardIndex

type BoardIndex        = GenBoardIndex Int
type BoardLookups = GenBoardIndex Remaining

-- Board
data Board = Board {
     _values :: Array (Int,Int) (Maybe Value),
     _lookups :: BoardLookups
     }

makeLenses ''Board

instance Show Board where
   show b = let arr = (b^.values)
            in zipWith (\a b -> map (a,) b) [1..9] (repeat [1..9])
             & map (map (foldr const '.' . (arr!)))
             & unlines

emptyboard = Board (listArray ((1,1),(9,9)) (repeat Nothing))
                   (BoardIndex s s s)
           where s = listArray (1,9) (repeat initSet)

makeBoardIndex :: Int -> Int  -> BoardIndex
makeBoardIndex a b
    | a < 1 || a > 9 = error "bad BoardIndex"
    | b < 1 || b > 9 = error "bad BoardIndex"
    | otherwise      = BoardIndex a b $ (((a-1)`div`3)*3+((b-1)`div`3)+1)


possibles :: Board -> BoardIndex -> Maybe (Set Value)
possibles board index = 
    case (board ^? values . ix (index^.row,index^.col)) of
         Just Nothing -> Just $
            foldl' intersection initSet $
             tzipWith (\a b -> a ^. ix b) (board ^. lookups) index
         _ -> Nothing

play :: Board -> (BoardIndex,Value) -> Board
play board (index,value) = board
     & set (values.ix (index^.row,index^.col)) (Just value)
     & over lookups ( 
         tzipWith (\ind -> over (ix ind) (deleteSet value)) index )

allIndices :: [BoardIndex]
allIndices = liftA2 (makeBoardIndex) idxs idxs
   where  idxs :: [Int]
          idxs = [1..9]

-- Is there a Lensy way to do this?
iterate :: Board -> Either (Either Board Board) Board
iterate b | not (null plays)  = Right $ foldl' play b plays
          | F.all isJust (b^.values) = Left (Right b)
          | otherwise = Left $ Left b 
  where plays :: [(BoardIndex,Value)]
        plays = allIndices
              & map (\x -> possibles b x >>= return . (x,))
              & map (\x -> do
                   (l,r) <- x
                   guard (length r == 1)
                   r1 <- foldr (const.Just) Nothing r 
                   return (l,r1)
                )
              & catMaybes


makeplays :: Board -> Board 
makeplays b = 
  case iterate b of
      Right b2 ->  makeplays b2
      Left (Right x) -> x
      Left (Left x) -> error (show x)

readboard :: Text -> Board
readboard t = foldl' play emptyboard lns
  where lns = lines t
            & map (map toVal . unpack) 
            & map ix1
            & ix2
            & concatMap (map (first (uncurry makeBoardIndex)))
        toVal '.' = Nothing
        toVal c = Just c
        ix1 :: [Maybe a] -> [Maybe (Int,a)]
        ix1 = zipWith (\a b -> (a,)<$> b) [1..9]
        ix2 = zipWith (\a b -> catMaybes b & map (first (a,))) [1..9]
        

main :: IO ()
main =  interact (pack . show . makeplays . readboard)
