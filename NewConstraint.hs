{-# LANGUAGE
    NoImplicitPrelude,
    MultiParamTypeClasses,
    ScopedTypeVariables,
    TypeFamilies,
    DefaultSignatures,
    ConstraintKinds,
    FlexibleContexts,
    GeneralizedNewtypeDeriving,
    ExplicitForAll
#-}

module NewConstraint (
       Continuation(Win,Continue),foldPlays,
       Board,Space,Piece,Move,serialise,playBoard,spaces,pieces,
       Game,GameBoard,raise,options,updates,nextplays,open,localSetup,
       makeGame, emptyBoard, runGame,
       BoardGame(BoardGame),Failable
       ) where

import ClassyPrelude
import Control.Monad.State


-- Game Winning
---------------
-- Losing should be carried in a foldable monadplus (see below)
data Continuation = Win | Continue deriving Show
instance Monoid Continuation where
  mempty = Continue
  mappend Win _ = Win
  mappend _ Win = Win
  mappend _ _ = Continue

foldPlays :: Monoid x => (Continuation,x) -> (Continuation,x) -> (Continuation,x)
foldPlays (Win,x) _ = (Win,x)
foldPlays c c' = mappend c c'

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
    (m ~ t m1, MonadTrans t, Game m1, Game (t m1),
    GameBoard (t m1) ~ GameBoard m1)

-- The Game Class

class (MonadPlus m, Board (GameBoard m)) => Game m where
   type GameBoard m :: *

   localSetup :: m ()
   localSetup = return ()
   
   setUp :: (GameBoard m) -> m ()
   default setUp :: StackedGame t m1 m => (GameBoard m) -> m()
   setUp xs = lift (setUp xs) >> localSetup >> updates (serialise xs) >> return ()


   options :: Space (GameBoard m) -> m (Set (Piece (GameBoard m)))
   default options :: StackedGame t m1 m =>
              Space (GameBoard m) -> m (Set (Piece (GameBoard m)))
   options = lift . options

   open :: m (Set (Space (GameBoard m)))
   default open :: StackedGame t m1 m => m (Set (Space (GameBoard m)))
   open = lift open

   -- updates should never call a lifted function
   updates :: [Move (GameBoard m)] -> m Continuation
   updates _ = return Continue

   -- Rules ::
   -- This needs to be a READ ONLY action.
   -- i.e. forall m, nextplays >> m == m
   -- ?? Can this be enforced through the type system ??
   -- NextPlays should have no effect
   -- nextplays >> m === m
   nextplays :: m [Move (GameBoard m)] 
   nextplays = return []

   -- The Two functions below are not exported!
   -- Play is internal - is not to be exported. Therefore, all Instances
   -- Either need to be monad transformers, or defined via Board

   raise :: (GameBoard m -> x) -> m x
   default raise :: StackedGame t m1 m => (GameBoard m -> x) -> m x
   raise = lift . raise 

   play :: Move (GameBoard m) -> m Continuation
   default play :: StackedGame t m1 m => Move (GameBoard m) -> m Continuation
   play x = liftM2 mappend (updates $ return x) (lift $ play x)
   
   sequenceMoves :: m (Continuation,[Move (GameBoard m)])
   default sequenceMoves :: StackedGame t m1 m =>
            m (Continuation,[Move (GameBoard m)])
   sequenceMoves = do
     plays <- nextplays
     if null plays
        then return (Continue,[])
        else do
          output <- sequence $ flip map plays $ \m -> do
                  c1 <- play m 
                  (c2,moves) <- lift sequenceMoves
                  return $ foldPlays (c1,[m]) (c2,moves)
          case foldl' foldPlays mempty output of
                  (Win,x) -> return (Win,x)
                  (Continue,x) -> foldPlays (Continue,x) <$> sequenceMoves
     
   lose :: m ()
   default lose :: StackedGame t m1 m => m ()
   lose = lift lose


-- Promoting a Board to a Game
------------------------------

type Failable m = (MonadPlus m, Foldable m)

newtype BoardGame b m a = BoardGame (StateT b m a)
        deriving (Functor,Applicative,Monad,Alternative, MonadPlus)

runGame :: forall m . forall b . forall a . (Monad m, Board b) =>  BoardGame b m a -> m b
runGame (BoardGame t) = execStateT t emptyBoard


instance (Board b, Failable m) => Game (BoardGame b m) where
         type GameBoard (BoardGame b m) = b

             
         setUp b = BoardGame $ put b

         raise f = BoardGame $ f <$> get
         open = raise spaces
         options _ = raise pieces
         updates xs = BoardGame (mapM (modify.playBoard) xs)
                   >> return Continue
         play = updates . return
         sequenceMoves = return (Continue,[])
         lose = BoardGame $ lift mzero

         
makeGame :: ( Game m, Board b, b ~ GameBoard m ) => b -> m (Continuation,GameBoard m)
makeGame b = do
   setUp b
   (c,_) <- sequenceMoves
   endgame <- raise id
   return (c,endgame)
