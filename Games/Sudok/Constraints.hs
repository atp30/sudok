{-# LANGUAGE
    NoImplicitPrelude,
    MultiParamTypeClasses,
    ScopedTypeVariables,
    TypeFamilies,
    DefaultSignatures,
    ConstraintKinds,
    FlexibleContexts,
    GeneralizedNewtypeDeriving,
    ExplicitForAll,
    TupleSections,
    TypeApplications,
    OverloadedStrings

#-}

module Games.Sudok.Constraints (
       Board, Space, Piece, Move, serialise, playBoard, boardMoves,
       GSpace, GPiece,
       Game, GameBoard, raise, options, updates, nextplays,  localSetup,
       makeGame, emptyBoard, runGame, execGames, lose, won, log, playName,
       BoardGame(BoardGame),Failable
       ) where

import ClassyPrelude hiding (log,Map)
import Data.Map.Strict (Map)
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import ListT (ListT)
import qualified ListT as LT
-- import Control.Monad.List

import qualified Data.Text as T
import Data.Function ((&))
-- Games
--------
type List a = [a]

type Move b = (Space b,Piece b)

class (Eq (Space b),Eq (Piece b), Ord (Space b), Ord (Piece b))
   => Board b where
   type Space b :: *
   type Piece b :: *
   serialise :: b -> [Move b]
   playBoard :: Move b -> b -> b
   boardMoves :: b -> Map (Space b) (Set (Piece b))
   emptyBoard :: b

type StackedGame t m1 m =
    (m ~ (t m1), MonadTrans t, Game m1, Game (t m1),
    GameBoard (t m1) ~ GameBoard m1)

type GSpace m = Space (GameBoard m)
type GPiece m = Piece (GameBoard m)
-- The Game Class
class (MonadPlus m, Board (GameBoard m)) => Game m where
   type GameBoard m :: *

   playName :: m Text
   playName = return "Unnamed"
   
   localSetup :: m ()
   localSetup = return ()

   -- updates should never call a lifted function
   updates :: [Move (GameBoard m)] -> m ()
   updates _ = return ()

   nextplays :: m [Move (GameBoard m)] 
   nextplays = return []

   options :: m (Map  (GSpace m) (Set (GPiece m)))
   default options :: StackedGame t m1 m
                   => m (Map  (GSpace m) (Set (GPiece m)))
   options = lift options

   -- The functions below are not exported!
   -- Play is internal - is not to be exported. Therefore, all Instances
   -- Either need to be monad transformers, or defined via Board
   log :: Text -> m ()
   default log :: StackedGame t m1 m => Text -> m ()
   log = lift . log

   setUp :: (GameBoard m) -> m ()
   default setUp :: StackedGame t m1 m => (GameBoard m) -> m()
   setUp xs = lift (setUp xs) >> localSetup >> updates (serialise xs) >> return ()

   raise :: (GameBoard m -> x) -> m x
   default raise :: StackedGame t m1 m => (GameBoard m -> x) -> m x
   raise = lift . raise 

   play :: Move (GameBoard m) -> m ()
   default play :: StackedGame t m1 m => Move (GameBoard m) -> m ()
   play x = liftM2 mappend (lift $ play x) (updates $ return x) 
    
   sequenceMoves :: m (List (Move (GameBoard m)))
   default sequenceMoves :: StackedGame t m1 m =>
            m (List (Move (GameBoard m)))
   sequenceMoves = do
     w <- won
     if w then return []
     else do
            x1s <- lift sequenceMoves
            updates x1s
            w <- won
            if w then return  []
            else do
              plays <- nextplays
              xs <- reverse <$> foldl' (\moves nextmove -> do
                                    w <- won
                                    if w then return mempty
                                        else do playName >>= log
                                                play nextmove 
                                                fmap (nextmove:) moves)
                                (return []) 
                                plays
              if null xs then return (x1s++xs)
                         else ((x1s++xs)++)<$>sequenceMoves
     
   lose :: Monoid a =>  m a
   default lose :: (StackedGame t m1 m, Monoid a) => m a
   lose = lift lose  


won :: Game m => m Bool
won = null  <$> options

-- Promoting a Board to a Game
------------------------------
type Failable m = (MonadPlus m, Foldable m)

newtype BoardGame b a = BoardGame (StateT b (ListT (Writer (CountSet Text))) a)
        deriving (Functor,Applicative,Monad,Alternative, MonadPlus)

runGame :: Board b =>  BoardGame b a -> (Maybe (a,b),CountSet Text) 
runGame (BoardGame t) = runWriter $ LT.head $ runStateT t emptyBoard

execGames :: Board b =>  BoardGame b a -> (Maybe b,Text)
execGames (BoardGame t) = second tshow $ runWriter $ LT.head $ execStateT t emptyBoard

instance (Board b) => Game (BoardGame b) where
         type GameBoard (BoardGame b) = b
         log           = BoardGame . lift . lift . tell . csSingleton
         setUp   b     = BoardGame $ put b
         raise   f     = BoardGame $ f <$> get
         options       = raise boardMoves
         updates xs    = BoardGame (mapM (modify.playBoard) xs) >> return ()
         play          = updates . return
         sequenceMoves = return []
         lose          = BoardGame $ lift mzero >> return mempty

         
makeGame :: ( Game m, Board b, b ~ GameBoard m ) => b -> m [Move b] 
makeGame b = setUp b >> raise id >> sequenceMoves 


-- Helper Type - for logging performance
-- Can be used to assess difficulty of sudokus
---------------------------------------------

newtype CountSet a = CountSet (Map a Int) deriving (Read)

csToList :: Ord a => CountSet a -> [(a,Int)]
csToList (CountSet a) = mapToList a

csSingleton :: Ord a => a -> CountSet a
csSingleton a = CountSet $ singletonMap a 1

instance Ord a => Monoid (CountSet a) where
   mempty = CountSet mempty
   mappend (CountSet a) (CountSet b) =
      CountSet $ unionWith (+) a b

pad :: Int -> String -> String 
pad n t = t ++ (replicate (n-length t) ' ')

instance (MonoFoldable a, Element a ~ Char, Ord a) => Show (CountSet a) where 
   show (CountSet a) = a
     & mapToList
     & sortOn snd
     & reverse
     & map (\(a,b) -> pad 30 (unpack a++":")++" "++show b)
     & unlines
