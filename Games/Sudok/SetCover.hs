{-#
LANGUAGE
        NoImplicitPrelude,
        GeneralizedNewtypeDeriving,
        TypeFamilies,
        TupleSections,
        TypeApplications,
        ScopedTypeVariables,
        GADTs,
        OverloadedStrings,
        FlexibleContexts
#-}

module Games.Sudok.SetCover where

import Games.Sudok.Constraints
import ClassyPrelude hiding (log,groupBy,Map,lefts)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import qualified Data.Foldable as F

import Control.Strat
import Data.PairSet hiding (fromList)

import Control.Monad.State.Strict (StateT, MonadState, get, put, modify, evalStateT, execState)
import Data.Function ((&))
import Control.Arrow ((>>>))


type Moves p s = DList (p,s)
type StratSink q m = Strategy q m q

cleanups :: (Ord p, Ord s) => Strategy (Moves p s) Game p s (Moves p s)
cleanups = makeStrategy () const $ \_ -> do
    xs <- lefts <$> get
    let pieces = M.filter ((==1).length)
               & maptoList
               & concatMap (sequence . fmap setToList)
    modify (flip (foldl' (flip deleteSet)) pieces)
    return pieces

cleanups :: (Ord p, Ord s) => Strategy (Moves p s) Game p s (Moves p s)
cleanups = makeStrategy () const $ \_ -> do
    xs <- toList <$> rights <$> get
    let pieces = M.filter ((==1).length)
               & maptoList
               & concatMap (sequence . fmap setToList)
    modify (flip (foldl' (flip deleteSet)) pieces)
    return pieces

removeplays :: (Ord p, Ord s)
            => StratSink (Moves p s) (Game p s)
removeplays = makeSink $ \(p,_) -> modify (removeLeft (singletonSet p))

onlyone :: (Ord p, Ord s)
            => Set p -> Set s -> (p->p')
            -> Strategy (Moves p s) (Game p s) (Map p' (Set s))
onlyone ps ss f = makeStrategy
        (M.fromListWith const (map ((,ss).f) $ setToList ps))
        (\m (p,s) -> M.adjust (deleteSet s) p m)   
        (\m -> do
             let ps = toList m
             forM_ ps ( \(p,ss)  -> modify (biRemove ((==p).f) (`elem`ss)))
             return mempty)


-- exactlyone :: (Ord p, Ord s)
--             => Set p -> Set s -> (p->p') ->
--             -> Strategy (Moves p s) (Game p s) (Map s (Set p'))
-- exactlyone ps ss f = makeStrategy
--         (M.fromListWith const (map ((,ss).f) $ setToList ps)
--         (\m (p,s) -> M.adjust (deleteSet s) p m)   
--         $ \m -> do
--              let ps = fromList p
--              forM_ ps $ \(p,ss)  -> modify (biRemove ((==p).f) (`elem`ss))
--              return mempty




-- need to make onlyone work with both left and right


-- oo :: (Ord p, Ord s, MonadState (Set p) m)
--    => Set p -> (p -> [Set p]) -> Strategy (Moves p s) m (Pairset (Set p) s)

-- consume :: (Ord p, Ord s, Ord p', Ord s') =>
--         (p -> [Set p]) -> Map (Set p) (Set s) -> (p,s) -> Map (Set p) (Set s) 
-- consume f m (p,s) = foldl' upd m $ f p
--     where  upd m p = M.update (go s) m' p'
--            go s st  =  let st'    = deleteSet s st
--                        in if onull st' then Nothing else Just st'

-- init :: (Ord p, Ord s, Ord p', Ord s',
--          SetContainer ps, ContainerKey ps ~ (p',s'))
--         => (p -> [Set p]) -> ps -> Map (Set p) (Set s)
-- init f = toList
--      >>> concatMap (\(p,s) -> fmap (,singletonSet s) (f p))
--      >>> fromListWith union

-- produce :: (Ord p, Ord s, IsSequence qs, (p,s) ~ Element qs) =>
--         Map (Set p) (Set s) -> qs
-- produce = toList
--       >>> \xs -> app xs <> app (map (\(a,b)->(b,a)) xs)
--     where app xs = do




-- onlyone' :: (Ord p, Ord s, Ord p', MonadState (Set p) m)
--         => Set p -> Set s -> (p -> [Either p' s'])
--            -> Strategy (Moves p s) m (Map p' (Set s), Map s' (Set p))

-- onlyone' ps ss f =
--          makeStrategy init (foldl' consume) produce
--   where init = M.fromListWith union $ do
--           p <- fromList ps
--           s <- fromList ss
--           (p',s') <- f p
--           return (p', singletonSet s')

--         consume m inc = foldl' (flip $ M.update go) (curry f inc)

--         go a b = let a' = deleteSet a b
--                  in if onull a' then Nothing else Just a'

--         produce m = do
--                 opts <- get


-- onlyone :: forall p s p' m . (Ord p, Ord s, Ord p', MonadState (PairSet p s) m)
--         => [p] -> Set s -> (p -> [p']) -> Strategy (Moves p s) m (Map p' (Set s))
-- onlyone init s f = makeStrategy (M.fromListWith const $ map (,s)
--                                                       $ concatMap f init)
--                                 (foldl' consume) produce
--   where consume m (p,s) = 
--                  foldl' (\m' p' -> M.update (go s) p' m') m $ f p

--         go v ss = let s' = v `deleteSet` ss
--                  in if onull s' then Nothing else Just s'

--         produce :: Map p' (Set s) -> m (Moves p s)
--         produce m = do
--             opts <- toList . lefts <$> get
--             let o = flip concatMap opts $ \x ->
--                     f x & map (flip lookup m)
--                         & intersects
--                         & \y -> if length y == 1 then
--                                         map (x,) $ toList y
--                                 else mempty
--             o & fromList & return

--         intersects :: [Maybe (Set s)] -> Set s
--         intersects [x] = foldr const mempty x
--         intersects (x:xs) = foldr const mempty x  `intersect` (intersects xs)
--         intersects [] = mempty


-- requireeach :: forall p s p' m . (Ord p,Ord s, Ord p',
--                                 MonadState (PairSet p s) m)


-- -- fhead :: (MonoFoldable fa, a ~ Element fa, Monoid a) => fa -> a
-- -- fhead = foldr const mempty

-- -- mapFromFoldableWith :: (Foldable f, Ord b) => (a -> a -> a) -> f (b,a) -> Map b a
-- -- mapFromFoldableWith f = F.foldl' (flip . uncurry $ insertWith f) mempty 

-- -- groupBy :: Ord b => (a -> [b]) -> [a] -> [[a]]
-- -- groupBy f xs =
-- --      let pairs = do
-- --             x <- xs
-- --             v <- f x
-- --             return (v,[x])
-- --      in map snd . mapToList $ mapFromFoldableWith (++) pairs

-- -- -- 1. Remove Options after a piece has been played
-- -- --------------------------------------------------

-- -- removePlayed :: (Game m, sq~GSpace m, v ~ GPiece m) =>
-- --              RemovePlayed sq v m a -> m a
-- -- removePlayed (RemovePlayed x) = evalStateT x mempty

-- -- newtype RemovePlayed sq v m a = RemovePlayed (StateT (Map sq (Set v)) m a)
-- --    deriving (Functor,Applicative,Alternative,
-- --              Monad,MonadPlus,
-- --              MonadTrans, MonadState (Map sq (Set v)))

-- -- instance (Game m, GSpace m ~ sq, sq ~ GSpace (RemovePlayed sq v m),
-- --                   GPiece m ~ v, v ~ GPiece (RemovePlayed sq v m), Show sq)
-- --       => Game (RemovePlayed sq v m) where
-- --    type GameBoard (RemovePlayed sq v m) = GameBoard m

-- --    localSetup = raise boardMoves >>= put
-- --    options = get
-- --    updates = mapM_ $ \(sq,_) -> do
-- --                 g <- get
-- --                 guard (sq `member` g) 
-- --                 modify $ deleteMap sq

-- -- -- 2. Prevent multiple 

-- -- newtype ForbidTwice x sq v m a = ForbidTwice (
-- --                 ReaderT (sq -> [x]) (
-- --                   StateT (Map x (Set v)) m) a)
-- --    deriving (Functor,Applicative,Alternative,Monad,MonadPlus)

-- -- instance MonadTrans (ForbidTwice x sq v) where
-- --   lift = ForbidTwice . lift . lift

-- -- forbidTwice :: (Game m, sq~GSpace m, v ~ GPiece m, Ord x) =>
-- --             (sq -> [x]) -> ForbidTwice x sq v m a -> m a
-- -- forbidTwice f (ForbidTwice x) = evalStateT (runReaderT x f) mempty

-- -- filterNextPlays :: Game m => m [(GSpace m,GPiece m)]
-- -- filterNextPlays = 
-- --        do o  <- options
-- --           return $ o
-- --                  & mapToList 
-- --                  & filter ((==1).length.snd)
-- --                  & concatMap (\(a,b)-> (a,)<$>foldr (const.return) [] b)

-- -- instance (Game m, GSpace m ~ sq, GPiece m ~ v, Ord x, Show x, Show sq, Show v) =>
-- --          Game (ForbidTwice x sq v m) where
-- --    type GameBoard (ForbidTwice x sq v m) = GameBoard m
-- --    playName = return "Prevent from occuring twice"
-- --    localSetup = do
-- --        ForbidTwice $ put mempty
-- --        f   <- ForbidTwice ask
-- --        mvs <- mapToList <$> raise boardMoves
-- --        let pairs = do
-- --               (key,val) <- mvs
-- --               views <- f key
-- --               return (views,val)
-- --        ForbidTwice $ put mempty
-- --        forM_ pairs $ \(k,v) ->
-- --           ForbidTwice $ modify (insertWith union k v)

-- --    updates = mapM_ $ \(sq,v) -> 
-- --       do view <- ForbidTwice ask
-- --          forM_ (view sq) $ \s -> do
-- --             x <- fhead . lookup s <$> ForbidTwice get
-- --             guard (v `elem` x)
-- --             ForbidTwice . modify $ adjustMap (deleteSet v) s

-- --    nextplays = filterNextPlays

-- --    options = do
-- --        f <- ForbidTwice ask
-- --        q <- ForbidTwice get
-- --        m <- lift options
-- --        return $ flip M.mapWithKey m $
-- --               \k v -> foldl' intersection v (fhead . flip lookup q <$> f k)


-- -- -- Require each element to appear once per view 
-- -- ----------------------------------------------

-- -- requireEach :: (Game m, sq~GSpace m, v ~ GPiece m, Ord x) =>
-- --             (sq -> [x]) -> RequireEach sq v x m a -> m a
-- -- requireEach f (RequireEach x) = runReaderT x f

-- -- newtype RequireEach sq v x m a = RequireEach (ReaderT (sq -> [x]) m a)
-- --    deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

-- -- instance MonadTrans (RequireEach sq v x)
-- --     where lift = RequireEach . lift

-- -- instance (Ord x, Game m, sq~ GSpace m, Show sq, Show v, Show x, v ~ GPiece m)
-- --      => Game (RequireEach sq v x m) where
-- --     type GameBoard (RequireEach sq v x m) = GameBoard m
-- --     playName = return "Require to occur once"
-- --     nextplays = do
-- --         view <- RequireEach ask
-- --         o <- mapToList <$> options
-- --         let pairs = do
-- --               (sq,pieces) <- o
-- --               piece <- setToList pieces
-- --               x <- view sq
-- --               return (x,[(piece,sq)])

-- --             bigmap = mapFromFoldableWith (++) pairs
-- --                    & map (map (second (return @[]))
-- --                       >>> mapFromFoldableWith (++)
-- --                       >>> M.filter ((==1).(length @[_]))
-- --                       )
-- --                    & M.filter (not.null)
-- --                    & M.elems
-- --                    & concatMap (mapToList
-- --                        >>> concatMap (\(a,bs)->(,a)<$>bs)
-- --                        )
-- --                    -- This is important, otherwise we try to play duplicates!
-- --                    & setToList @(Set _) . setFromList

-- --         return bigmap

-- -- exactlyOnce :: (Ord x, Game m, sq ~ GSpace m, v ~ GPiece m, Show sq, Show x, Show v)
-- --             => (sq -> [x]) -> FilterMultiOptions sq v x (
-- --                               RequireEach sq v x (
-- --                               ForbidTwice x sq v m)) a -> m a
-- -- exactlyOnce f = filterMultiOptions f >>> requireEach f >>> forbidTwice f

-- -- -- RemoveDuplicates!
-- -- -- If there are N cells which monopolise N values, then 
-- -- -- those values cannot go anywhere else

-- -- -- This works, but is damned inefficient
 
-- -- filterMultiOptions :: (Game m, sq~GSpace m, v ~ GPiece m, Ord x) =>
-- --             (sq -> [x]) -> FilterMultiOptions sq v x m a -> m a
-- -- filterMultiOptions f (FilterMultiOptions x) = runReaderT x f

-- -- newtype FilterMultiOptions sq v x m a = FilterMultiOptions (ReaderT (sq -> [x]) m a)
-- --    deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

-- -- instance MonadTrans (FilterMultiOptions sq v x)
-- --     where lift = FilterMultiOptions . lift

-- -- instance (Ord x, Game m, sq~ GSpace m, Show sq, Show v, Show x, v ~ GPiece m)
-- --      => Game (FilterMultiOptions sq v x m) where
-- --     type GameBoard (FilterMultiOptions sq v x m) = GameBoard m
-- --     playName = return "Filter Multi Options"
-- --     options = do
-- --       o <- lift options
-- --       v <- FilterMultiOptions ask
-- --       let maps :: [[(sq,Set v)]]
-- --           maps = groupBy (v.fst) (mapToList o)

-- --           newoptions :: [(sq,Set v)]
-- --           newoptions = flip concatMap maps $ \xs ->
-- --                     xs & map (uncurry (flip (,)) >>> second  (return @[]))
-- --                        & mapFromFoldableWith (++)
-- --                        & mapToList
-- --                        & filter (uncurry (==) . (length *** length))
-- --                        & map (\(st,vals) -> do
-- --                                 (x,opts) <- xs
-- --                                 guard $ not (x `elem` vals) 
-- --                                 return (x,opts `difference` st)
-- --                                 )
-- --                        & concat
                       
-- --             -- flip execState xs $ do
-- --               -- forM_ (groupBy (return.snd) xs) $ \x@((_,y):_) ->
-- --               --   if length x == length y
-- --               --   then modify $ map (\a ->
-- --               --           if a `elem` x
-- --               --               then a
-- --               --               else second (`difference` y) a)
-- --               --   else return ()
-- --       return $ foldl' (\m (k,val) -> insertWith (intersection) k val m) o newoptions

-- --     nextplays = filterNextPlays

-- -- -- Make Guesses
-- -- ---------------

-- -- newtype Guess sq v m a = Guess (m a)
-- --     deriving (Functor,Applicative,Alternative,Monad, MonadPlus)

-- -- instance MonadTrans (Guess sq v) where
-- --     lift = Guess 

-- -- instance (Game m, sq ~ GSpace m, v ~ GPiece m, Show sq, Show v)
-- --        => Game (Guess sq v m) where
-- --     type GameBoard (Guess sq v m) = GameBoard m
-- --     playName = return "Guess when stuck"
-- --     nextplays = do
-- --         w <- won
-- --         if w then return []
-- --         else do
-- --           -- opts <- take 1 . setToList <$>  open
-- --           opts <- mapToList <$> options
-- --           let opt3s = dropWhile (null.snd) $ sortOn (length.snd) opts
-- --           case takeWhile ((==1).length.snd) opt3s of
-- --                 [] -> do -- NEED TO CHECK IF WE'VE WON
-- --                       (a,y) <- foldr (const.return) mzero opt3s
-- --                       foldr (mplus.return) mzero $ (return.(a,)) <$> setToList y
-- --                 xs  -> 
-- --                     return $  concatMap (\(x,y)->(x,)<$>setToList y) xs 

-- -- makeGuesses :: Guess sq v m a -> m a
-- -- makeGuesses (Guess m)  = m
