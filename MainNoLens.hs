-- N.B. This causes a GHC panic on 8.0.2
--      Advise running on 8.2.1

{-# LANGUAGE
    MultiParamTypeClasses,
    TupleSections,
    ScopedTypeVariables,

    TypeFamilies,
    GeneralizedNewtypeDeriving
#-}

module Main where

import ConstraintsNoLens

import ClassyPrelude
import Prelude (readsPrec,read,interact)

import Control.Monad.State.Class
import Control.Monad.State hiding (sequence_)
import Control.Monad.Trans

import Control.Arrow hiding (first,second)
import Data.Function ((&))


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

newtype Square = Square (Digit,Digit) deriving (Read,Show,Ord,Eq,Bounded)

-- The Board
---------------------------------------------------
newtype Board = Board (Map Square Value)  deriving (Monoid)
instance Show Board where
   show (Board bs) = digits
           & map ((\a -> (a,)<$> digits)
               >>>concatMap (\x -> case lookup (Square x) bs of
                              Nothing -> "."
                              Just x -> show x
                            ))
           & unlines


-- Not a well behaved instance - messes with the remaining string
instance Read Board where
   readsPrec _ xs = filter (`elem`('.':['1'..'9'])) xs
                  & splitAt 81
                  & first ( zip (Square <$> liftM2 (,) digits digits)
                        >>> filter ((/='.').snd)
                        >>> map (second (Value .read.return))
                        >>> mapFromList
                        >>> Board)
                  & return

serialise :: Board -> [(Square,Value)]
serialise (Board b) = mapToList b

deserialise :: [(Square,Value)] -> Board
deserialise xs = Board (mapFromList xs)

newtype BoardState a = B (State Board  a)
        deriving (Monad,MonadState Board,Functor,Applicative)

bmap :: (Map Square Value -> Map Square Value) -> Board -> Board
bmap f (Board x) = Board (f x)


instance Game BoardState where
     type Space BoardState = Square
     type Piece BoardState = Value

     evalGame (B s) = flip evalState (mempty) s

     initialise = put mempty
     updatePlay [] = do
          Board size <- get
          return $ if length size>= 81 then Won else Incomplete

     updatePlay ((s,v):xs) = modify (bmap $ insertMap s v)
                          >> updatePlay xs

     play square value = do
         (Board board) <- get
         if square `member` board 
            then return $ Lost "already played"
            else updatePlay [(square,value)]

     plays = (,[]) <$> updatePlay []
     -- This is a list of squares - options shold be a list of plays

     options = liftM2 (,) digits digits
             & map Square
             & map (,ds)
             & return
               where ds =setFromList $ Value <$> digits

     previousplays = do
            (Board m) <- get
            return $ mapToList m


-- Preventing us from playing what we've already played
-------------------------------------------------------


-- This could be made more efficient by avoiding the call to the enumeration below
newtype RemovePlayed sq m a = RemovePlayed (StateT (Set sq) m a)
        deriving (Monad,MonadTrans,MonadState (Set sq),Applicative,Functor)
instance (Game m, Space m ~ sq, Ord sq, Eq sq)
         => Game (RemovePlayed sq m) where
      type Space (RemovePlayed sq m) = Space m
      type Piece (RemovePlayed sq m) = Piece m 

      evalGame (RemovePlayed m) = evalGame $ evalStateT m mempty

      initialise = do
         lift initialise
         a <- lift options 
         put (setFromList (fst <$> a))

      updatePlay s = modify (`difference` (setFromList$ map fst s))
                  >> return Incomplete
      
      options = do
        unplayed <- get
        oldoptions <- lift options
        return $ filter ((`member` unplayed).fst) oldoptions
           

-- Dealing with Rows, Columns and the LIke. Make a class of VIEWS
-- =================================================================
class View a x where
         view :: a -> x

newtype Column = Column Digit deriving (Read,Show,Ord,Eq)
instance View Square Column where view (Square (x,_)) = Column x

newtype Row = Row Digit deriving (Read,Show,Ord,Eq)
instance View Square Row where view (Square (_,y)) = Row y

newtype Box = Box Digit deriving (Read,Show,Ord,Eq)
instance View Square Box where
   view    (Square (Digit x, Digit y)) = Box $ Digit (((x-1)`div`3)*3+((y-1)`div`3)+1)

-- Ensuring No Duplicates within a View
---------------------------------------

-- This is what causes GHC Panic in 8.0.2
-- But Would rather pass through the type system --

newtype NoViewDuplicates x s v m a = NVD (StateT (Map x (Set v)) m a)
        deriving (Monad,MonadTrans,MonadState (Map x (Set v)), Functor, Applicative)
instance (View sq x, Game m, Eq x, Ord x, Eq v, Ord v, v ~ Piece m, sq ~ Space m) =>
         Game (NoViewDuplicates x sq v m) where
      type Space (NoViewDuplicates x sq v m) = sq
      type Piece (NoViewDuplicates x sq v m) = v
      -- Need a better way of passing options

      evalGame (NVD m) = evalGame $ evalStateT m mempty

      initialise = do
         lift initialise
         opts <- lift options
         put $ foldl' (flip $ uncurry $ insertWith union) mempty
             $ map (first view) opts
         a <- lift previousplays
         updatePlay a
         return ()

      updatePlay s = do
         actions <- sequence $ flip map s $ \(k,v) -> do
                m <- get
                let kk = view k
                let l = foldr const mempty $ lookup kk m
                if v `elem` l
                  then do modify $ adjustMap (deleteSet v) kk
                          return Incomplete
                  else return $ Lost "not an option"
         return $ foldl' combineState Incomplete actions

      plays = do
        opts <- options
        let m :: Maybe [(sq,v)]
            m = filter ((==1).length.snd) opts
              & map (\(sq,x)-> (sq,) <$> foldr (const.Just) Nothing x)
              & sequenceA -- Nothing if there's anywhere unplayable
        case m of
              Nothing -> return (Incomplete,[])
              Just xs -> do
                  ys <- sequence $ map (uncurry play) xs
                  (i,p2) <- lift plays
                  case i of
                       Incomplete -> do
                              updatePlay p2
                              return (foldl' combineState i ys, xs ++ p2)
                       _ -> return (i,p2)

      options = do
        opts <- lift options
        sequence $ flip map opts $ \(k,vs) -> do
            g <- get
            let v2 = foldr const mempty $ lookup (view k) g
            return (k, intersection vs v2 )


newtype RequireEach x s v m a = RE m a
        deriving (Monad,MonadTrans, Functor, Applicative)
instance (View sq x, Game m, Eq x, Ord x, Eq v, Ord v, v ~ Piece m, sq ~ Space m) =>
         Game (RequireEach x sq v m) where
      type Space (RequireEach x sq v m) = sq
      type Piece (RequireEach x sq v m) = v
      -- Need a better way of passing options

      evalGame (RequireEach m) = evalGame $ evalStateT m mempty

      initialise = do
         lift initialise
         a <- lift previousplays
         updatePlay a
         return ()

      plays = do
        opts <- options
        -- Each Item in Options which is Listed but Only Listed Once
        opts & foldl'  insertWith
        -- let m :: Maybe [(sq,v)]
        --     m = filter ((==1).length.snd) opts
        --       & map (\(sq,x)-> (sq,) <$> foldr (const.Just) Nothing x)
        --       & sequenceA -- Nothing if there's anywhere unplayable
        -- case m of
        --       Nothing -> return (Incomplete,[])
        --       Just xs -> do
        --           ys <- sequence $ map (uncurry play) xs
        --           (i,p2) <- lift plays
        --           case i of
        --                Incomplete -> do
        --                       updatePlay p2
        --                       return (foldl' combineState i ys, xs ++ p2)
        --                _ -> return (i,p2)
        
-- Test Case
------------
-- Need a way to load in the default
-- At the moment lots of serialisation and deserialisation
main :: IO ()
main = interact $ \x -> show $ map (fmap (deserialise . (++(serialise (read x))) ))
        $ flip runGames (serialise (read x)) $ 
          ( initialise :: NoViewDuplicates Column Square Value
                        (NoViewDuplicates Row Square Value
                        (NoViewDuplicates Box Square Value
                        (RemovePlayed Square BoardState
                        ))) ()
          )

           
        
