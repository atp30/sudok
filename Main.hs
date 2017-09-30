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
import Data.Array.Lens
import qualified Data.Foldable as F
import Data.Function ((&))
import Data.Text.IO (interact)
import Data.Traversable (mapAccumL)

import Control.Lens
import Control.Arrow ((***))
import Control.Applicative



-- A Traversable Version of Zip
-- From SO:
-- https://stackoverflow.com/questions/41522422

tzipWith f xs ys = val
    where Just val = sequenceA . snd . mapAccumL pair (F.toList xs) $ ys
          pair [] y = ([],Nothing)
          pair (x:xs) y = (xs, Just $ f x y)

initSet :: Set Value
initSet = setFromList ['1'..'9']

indexlist = [1..9]

-- Basic Container Types
type Value     = Char
type Remaining = (Array Int (Set Value))

-- BoardIndex Types - allow us to map over rows, columns and boxes
-- we need to be able to BoardIndex an arbitrary column by
-- other elements in the same row, column, and box
data GenBoardIndex a = BoardIndex { _row::a, _col::a, _box::a}
     deriving (Functor,Foldable,Traversable,Show)

type instance Element    (GenBoardIndex a) = a
instance MonoFunctor     (GenBoardIndex a)
instance MonoFoldable    (GenBoardIndex a)
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

unBoardIndex :: BoardIndex -> (Int,Int)
unBoardIndex i = (_row i, _col i) 

play :: Board -> (BoardIndex,Value) -> Board
play board (index,value) = board
     & set (values.ix (unBoardIndex index)) (Just value)
     & over lookups ( 
         tzipWith (\ind -> over (ix ind) (deleteSet value)) index )

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

allIndices :: [BoardIndex]
allIndices = liftA2 (makeBoardIndex) idxs idxs
   where  idxs :: [Int]
          idxs = [1..9]

-- Only Possible Cell
-- A cell is the only possible cell for a value if
-- there is a value in it's list of possibles that is not
-- in the list of possibles for any remaining row, column, or box
counterparts :: BoardIndex -> GenBoardIndex [(Int,Int)]
counterparts BoardIndex{_row=r,_col=c,_box=b} =
    BoardIndex (map (r,) $ filter (/=c) [1..9])
               (map (,c) $ filter (/=r) [1..9])
               (filter (/=(r,c)) [(x+bx,y+by) | x <- [1..3], y <- [1..3]])
      where bx = ((b-1)`div` 3)*3
            by = ((b-1)`mod` 3)*3

-- I should build in more sharing to this, rather than rebuilding from scratch
allCounterparts = array ((1,1),(9,9))
                $ map (id &&& counterparts . uncurry makeBoardIndex)
                $ range ((1,1),(9,9))

-- Find All Possible locations for a value
possibles :: Board -> BoardIndex -> Either Value (Set Value)
possibles board index = 
    case (board ^? values . ix (index^.row,index^.col)) of
         Just Nothing -> Right $
            foldl' intersection initSet $
             tzipWith (\a b -> a ^. ix b) (board ^. lookups) index
         Just (Just x) -> Left x
         Nothing -> error "should never happen (possibles)"

allPossibles :: Board -> Array (Int,Int) (Either Value (Set Value))
allPossibles b = array ((1,1),(9,9))
                $ map (id &&& possibles b . uncurry makeBoardIndex)
                $ range ((1,1),(9,9))

upset :: Ord a => Either a (Set a) -> Set a
upset (Left a) = setFromList [a]
upset (Right a) = a


-- Is there a Lensy way to do this?
iterate :: Board -> Either (Either Board Board) Board
iterate b | not (null plays)  = Right $ foldl' play b plays
          | F.all isJust (b^.values) = Left (Right b)
          | otherwise = Left $ Left b 
  where possibleArr = allPossibles b
        plays :: [(BoardIndex,Value)]
        plays = assocs possibleArr
              & map (second $ foldr const mempty)
              & filter (not.null.snd)
              & map (\(l,r) -> fmap (uncurry makeBoardIndex l,) (getplay l) <|> do
                   guard (length r == 1)
                   r1 <- foldr (const.Just) Nothing r 
                   return (uncurry makeBoardIndex l,r1)
                )
              & catMaybes
        getplay :: (Int,Int) -> Maybe Value
        getplay x = (allCounterparts ! x)
                  & over (traversed . traversed) (possibleArr !)
                  & over (traversed) (difference initSet
                                     .foldl' union mempty
                                     .map upset)
                  & F.foldl' union mempty
                  & foldr (const.Just) Nothing



makeplays :: Board -> Board 
makeplays b = 
  case iterate b of
      Right b2 ->  makeplays b2
      Left (Right x) -> x
      Left (Left x) -> error (show x)

        

main :: IO ()
main =  interact (pack . show . makeplays . readboard)
