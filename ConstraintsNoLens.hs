-- Lens free veriosn of constraints.hs
{-# LANGUAGE
    NoImplicitPrelude,
    MultiParamTypeClasses,
    ScopedTypeVariables,
    TypeFamilies,
    DefaultSignatures,
    ConstraintKinds
#-}

module ConstraintsNoLens where
import ClassyPrelude
import Control.Monad.State.Class
import Control.Monad.Trans.List

-- A game is a monad which allows you to
--      play a move
--      check if you've won
--      check what possible next moves there are

data GameState = Won
               | Lost String
               | Incomplete deriving Show

combineState (Lost s) (Lost s2) = Lost (s ++ ", " ++ s2)
combineState (Lost s) _    = Lost s
combineState Won  _    = Won
combineState _    (Lost s) = Lost s
combineState _    Won  = Won
combineState _    _    = Incomplete

type GameParent t m1 m =
    (m ~ t m1, MonadTrans t, Game m1, Game (t m1),
    Space (t m1) ~ Space m1,
    Piece (t m1) ~ Piece m1)

-- This doesn't do my nondeterminism correctly
-- How should I do it?
runGames :: (Ord x, x ~ Piece m, Game m) =>
         m () -> [(Space m, Piece m)] -> [Either String [(Space m,Piece m)]]
runGames m xs = evalGame
             $  runListT
             $ (ListT $
                m >> sequence (map (uncurry play) xs) >> return [()])
                >> makeplay 
  where 
        makeplay = do
          (gs,_) <- lift plays
          case gs of
              Won         -> Right <$> lift previousplays 
              Lost s      -> return $ Left $ "Lost1 " ++ s
              Incomplete  -> do
                a <- lift options
                case a of
                    [] -> empty
                    ((x,vs):_) -> do
                        v <- ListT (return $ setToList vs)
                        a <- lift (play x v)
                        case a of
                            Won -> Right <$> lift previousplays
                            Lost s -> return $ Left $ "Lost2 " ++ s
                            Incomplete -> makeplay

     


class Monad m => Game m where
  type Space m :: *
  type Piece m :: *

  evalGame :: m a -> a

  -- I'm  not sure about this - maybe we just take a map of prevous plays
  -- playGame ::  m a -> (a)
  -- default playGame :: (MonadTrans t, m ~ t m1) => m a -> a
  -- playGame m = playGame 

  initialise :: m () -- Allows us to construct monad's that wrap others

  updatePlay :: [(Space m,Piece m)] -> m GameState
  updatePlay xs = return Incomplete

  -- play will take a value, modify and report on the gamestate

  -- plays will make all possible unsupervised plays, returning
  -- a new gamestate when no more decisions can be maed
  plays :: m (GameState,[(Space m,Piece m)])
  default plays :: GameParent t m1 m => m (GameState,[(Space m,Piece m)])
  plays = defaultPlays

  play :: Space m -> Piece m -> m GameState
  default play :: GameParent t m1 m => Space m -> Piece m -> m GameState
  play k v = do
       x <- lift $ play k v
       y <- updatePlay [(k,v)]
       return $ combineState y x

  options :: m [(Space m,Set (Piece m))]
  default options :: GameParent t m1 m => m [(Space m,Set (Piece m))]
  options = lift options

  previousplays :: m [(Space m,Piece m)]
  default previousplays :: GameParent t m1 m => m [(Space m,Piece m)]
  previousplays = lift previousplays

defaultPlays :: GameParent t m1 m => m (GameState, [(Space m, Piece m)])
defaultPlays = do
      (v,vs) <- lift plays
      a <- updatePlay vs
      case (v,a) of
        (Incomplete,Incomplete) -> do
           if not (null vs)
                then fmap (vs++) <$> plays
                else return (Incomplete,vs)
        -- This allows stuff to get out of sync!
        _ -> return (combineState v a, vs)


