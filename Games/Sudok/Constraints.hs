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
    TupleSections
#-}

module Games.Sudok.Constraints (
       Board,Space,Piece,Move,serialise,playBoard,spaces,pieces,
       Game,GameBoard,raise,options,updates,nextplays,open,localSetup,
       makeGame, emptyBoard, runGame, execGame, lose, won,
       BoardGame(BoardGame),Failable
       ) where

import ClassyPrelude
import Control.Monad.State

-- Games
--------
type Move b = (Space b,Piece b)

class (Eq (Space b),Eq (Piece b), Ord (Space b), Ord (Piece b))
   => Board b where
   type Space b :: *
   type Piece b :: *
   serialise :: b -> [Move b]
   playBoard :: Move b -> b -> b
   spaces :: b -> Set (Space b)
   pieces :: b -> Set (Piece b)
   emptyBoard :: b

type StackedGame t m1 m =
    (m ~ (t m1), MonadTrans t, Game m1, Game (t m1),
    GameBoard (t m1) ~ GameBoard m1)

-- The Game Class
class (MonadPlus m, Board (GameBoard m)) => Game m where
   type GameBoard m :: *

   localSetup :: m ()
   localSetup = return ()

   options :: Space (GameBoard m) -> m (Set (Piece (GameBoard m)))
   default options :: StackedGame t m1 m =>
              Space (GameBoard m) -> m (Set (Piece (GameBoard m)))
   options = lift . options

   open :: m (Set (Space (GameBoard m)))
   default open :: StackedGame t m1 m => m (Set (Space (GameBoard m)))
   open = lift open

   -- updates should never call a lifted function
   updates :: [Move (GameBoard m)] -> m ()
   updates _ = return ()

   nextplays :: m [Move (GameBoard m)] 
   nextplays = return []

   -- The Two functions below are not exported!
   -- Play is internal - is not to be exported. Therefore, all Instances
   -- Either need to be monad transformers, or defined via Board
   setUp :: (GameBoard m) -> m ()
   default setUp :: StackedGame t m1 m => (GameBoard m) -> m()
   setUp xs = lift (setUp xs) >> localSetup >> updates (serialise xs) >> return ()

   raise :: (GameBoard m -> x) -> m x
   default raise :: StackedGame t m1 m => (GameBoard m -> x) -> m x
   raise = lift . raise 

   play :: Move (GameBoard m) -> m ()
   default play :: StackedGame t m1 m => Move (GameBoard m) -> m ()
   play x = liftM2 mappend (lift $ play x) (updates $ return x) 
   
   sequenceMoves :: m [Move (GameBoard m)]
   default sequenceMoves :: StackedGame t m1 m =>
            m [Move (GameBoard m)]
   sequenceMoves = do
     w <- won
     if w then return []
     else do
            plays <- nextplays
            xs <- reverse <$> foldl' (\moves nextmove -> do
                                  w <- won
                                  if w then return []
                                       else do play nextmove 
                                               fmap (nextmove:) moves)
                              (reverse <$> lift sequenceMoves) 
                              plays
            if null xs then return xs
                       else (xs++)<$>sequenceMoves
     
   lose :: Monoid a =>  m a
   default lose :: (StackedGame t m1 m, Monoid a) => m a
   lose = lift lose  


won :: Game m => m Bool
won = null <$> open

-- Promoting a Board to a Game
------------------------------
type Failable m = (MonadPlus m, Foldable m)

newtype BoardGame b m a = BoardGame (StateT b m a)
        deriving (Functor,Applicative,Monad,Alternative, MonadPlus)

runGame :: forall m . forall b . forall a . (Monad m, Board b) =>  BoardGame b m a -> m (a,b) 
runGame (BoardGame t) = runStateT t emptyBoard

execGame :: forall m . forall b . forall a . (Monad m, Board b) =>  BoardGame b m a -> m b
execGame (BoardGame t) = execStateT t emptyBoard

instance (Board b, Failable m) => Game (BoardGame b m) where
         type GameBoard (BoardGame b m) = b
             
         setUp b       = BoardGame $ put b
         raise f       = BoardGame $ f <$> get
         open          = raise spaces
         options _     = raise pieces
         updates xs    = BoardGame (mapM (modify.playBoard) xs) >> return ()
         play          = updates . return
         sequenceMoves = return []
         lose          = BoardGame $ lift mzero >> return mempty

         
makeGame :: ( Game m, Board b, b ~ GameBoard m ) => b -> m [Move b] 
makeGame b = setUp b >> (raise id) >> sequenceMoves 
