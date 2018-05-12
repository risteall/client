{-# LANGUAGE TupleSections, NamedFieldPuns, DeriveGeneric, DeriveAnyClass #-}

module Match(MoveSet(..), currentMove, makeMoveSet, setCapture, toggleCapture, matchArrows) where

import Data.Function
import Data.Ord(comparing)
import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Array.IArray
import GHC.Generics (Generic)
import Control.DeepSeq
import Control.Monad
import Data.Bifunctor

import Base

multiIntersect :: Ord a => [a] -> [a] -> [a]
multiIntersect [] _ = []
multiIntersect _ [] = []
multiIntersect (a:as) (b:bs) = case compare a b of
  LT -> multiIntersect as (b:bs)
  EQ -> a : multiIntersect as bs
  GT -> multiIntersect (a:as) bs

multiUnion :: Ord a => [a] -> [a] -> [a]
multiUnion [] bs = bs
multiUnion as [] = as
multiUnion (a:as) (b:bs) = case compare a b of
  LT -> a : multiUnion as (b:bs)
  EQ -> a : multiUnion as bs
  GT -> b : multiUnion (a:as) bs

repartition :: Ord a => ([a], [a]) -> [a] -> ([a], [a])
repartition (m, o) c = let m' = multiIntersect m c in (m', multiUnion (sort (m ++ o)) c \\ m')

----------------------------------------------------------------

-- calculation of spareSteps is inaccurate
-- could be better discrimination of relevance e.g. steps going away are irrelevant
-- could fuse step and drag calculation
relevantAtoms :: Board -> Colour -> Map Piece [Arrow] -> [Square] -> Map Piece [Arrow] -> Int -> ([Step], [(Step, Step)])
relevantAtoms board player match traps accum n
    | spareSteps < 0 = ([], [])
    | spareSteps >= 0 = (singleStepsFrom relevantSquares board player, dragsFrom relevantSquares board player)
  where
    f :: (Piece, [Arrow]) -> (Int, Int)
    f (piece, as) | fst piece == player = (x, 0)
                  | otherwise = (0, x)
      where x = max 0 $ sum (map arrowLength as) - maybe 0 (sum . map arrowLength) (Map.lookup piece accum)
    (friend, enemy) = unzip $ map f (Map.assocs match)
    spareSteps = n - (max (sum friend) (2 * sum enemy))

    arrowToSquares ((x1,y1),(x2,y2)) = range ((x,y), (x',y')) \\ [(x2,y2)]
      where (x,x') = if x1 <= x2 then (x1,x2) else (x2,x1)
            (y,y') = if y1 <= y2 then (y1,y2) else (y2,y1)

    spread = nub . concatMap (\(x,y) -> [(i,j) | i <- [x-1,x,x+1], j <- [y-1,y,y+1], inRange boardRange (i,j)])
    
    usefulSquares :: Map Piece [Square]
    usefulSquares = Map.mapWithKey f match
      where f (c,_) as | c == player = sqs
                       | otherwise = spread sqs
              where sqs = foldl' union [] $ map arrowToSquares as

    relevantSquares = iterate spread (union traps (Map.foldl' union [] usefulSquares)) !! (2 * spareSteps)

matchArrows :: [Arrow] -> [Arrow] -> Bool
matchArrows [] _ = True
matchArrows ((source,dest):as) move = case partition ((== source) . fst) move of
  ([(_,d)], move') | d == dest -> matchArrows as move'
                   | otherwise -> matchArrows ((d, dest) : as) move'
  _ -> False

match :: Map Piece [Arrow] -> [Square] -> Map Piece [Arrow] -> [Square] -> Bool
match match traps move captures = all (\(p, as) -> maybe False (matchArrows as) (Map.lookup p move)) (Map.assocs match)
                                  && all (`elem` captures) traps

type MoveInfo = (Move, Board, Map Piece [Arrow], Map Square [Piece])

matchingMoves :: Board -> Colour -> [Arrow] -> [Square] -> [MoveInfo]
matchingMoves _ _ [] [] = []
matchingMoves initialBoard player arrows traps
    | Just _ <- matchMap = g initialBoard Map.empty Map.empty stepsPerMove
    | otherwise = []
  where
    matchMap :: Maybe (Map Piece [Arrow])
    matchMap = foldl' f (Just Map.empty) arrows
      where f (Just m) a@(source,_) | Just p <- initialBoard ! source
                                      = Just $ Map.insertWith (++) p [a] m
            f _ _ = Nothing
    Just mm = matchMap

    f, g :: Board -> Map Piece [Arrow] -> Map Square [Piece] -> Int -> [MoveInfo]
    f board accum accumCaps n | match mm traps accum (Map.keys accumCaps) && board /= initialBoard
                                  = (Move [], board, accum, fmap sort accumCaps) : g board accum accumCaps n
                              | otherwise = g board accum accumCaps n
    g board accum accumCaps n = do
      let (ss, ds) = relevantAtoms board player mm traps accum n
      steps <- map (:[]) (if n>=1 then ss else []) ++ map (\(a,b) -> [a,b]) (if n>=2 then ds else [])
      let (board', padded) = padMove board steps
          accum' = Map.unionWith addArrows accum (moveToArrows padded)
          accumCaps' = Map.unionWith (\a b -> sort (a ++ b)) accumCaps (moveToCaptureSet padded)
      (move, b, acc, accCaps) <- f board' accum' accumCaps' (n - length steps)
      return $ (appendMoves padded move, b, acc, accCaps)
    
----------------------------------------------------------------

sortGroupBy :: (a -> a -> Ordering) -> [a] -> [[a]]
sortGroupBy f = groupBy (\x y -> f x y == EQ) . sortBy f

mins :: Ord b => (a -> b) -> [a] -> ([a], b)
mins f (x:xs) = foldl' h ([x], f x) xs
  where
    h (l, e) a = let e' = f a in case compare e' e of
      LT -> ([a], e')
      EQ -> (a : l, e)
      GT -> (l, e)

-- eval1 applied before bucketing
-- note choice to use all arrows rather than the remainder after matching
-- zero-length arrows are excluded because there is no way to input them
eval1 :: MoveInfo -> Int
eval1 (_,_,x,_) = sum $ fmap (length . filter (uncurry (/=))) x

type BucketVal = (Int, (Int, Int))

eval2 :: Colour -> MoveInfo -> BucketVal
eval2 player (move,_,_,caps) = (nSteps move, foldl' f (0,0) (foldl' (++) [] caps))
  where f (x,y) (c,_) | c == player = (x+1, y)
                      | otherwise = (x, y-1)

sortMoves :: Colour -> [MoveInfo] -> [(Map Square [Piece], (Maybe Move, BucketVal))]
sortMoves _ [] = []
sortMoves player ms = map f $ sortGroupBy (comparing (\(_,_,_,x) -> x)) $ fst $ mins eval1 ms
  where
    f :: [MoveInfo] -> (Map Square [Piece], (Maybe Move, BucketVal))
    f bucket = case first (nubBy ((==) `on` (\(_,b,_,_) -> b))) $ mins (eval2 player) bucket of
      ([(pm,_,_,caps)], x) -> (caps, (Just pm, x))
      (((_,_,_,caps):_), x) -> (caps, (Nothing, x))

----------------------------------------------------------------

classifyCaptures :: [Map Square [Piece]] -> Map Square ([Piece], [Piece])
classifyCaptures [] = Map.empty
classifyCaptures (x:xs) = foldl' (Map.mergeWithKey (\_ (m, o) c -> Just (repartition (m, o) c))
                                                   (fmap (\(m, o) -> ([], sort (m ++ o))))
                                                   (fmap ([],)))
                                 (fmap (,[]) x)
                                 xs

capturesToFlags :: Map Square ([Piece], [Piece]) -> Map Square [Piece] -> Map Square [Bool]
capturesToFlags = Map.mergeWithKey (\_ (m,o) ps -> Just (f o (ps \\ m))) (fmap (map (const False) . snd)) (const Map.empty)
  where f :: [Piece] -> [Piece] -> [Bool]
        f [] _ = []
        f (a:as) (b:bs) | a == b = True : f as bs
        f (a:as) bs = False : f as bs

flagsToCaptures :: Map Square ([Piece], [Piece]) -> Map Square [Bool] -> Map Square [Piece]
flagsToCaptures = Map.intersectionWith (\(m,o) fs -> sort (m ++ [x | (x,f) <- zip o fs, f]))

----------------------------------------------------------------

data MoveSet = MoveSet {moveMap :: [(Map Square [Bool], (Maybe Move, BucketVal))]
                       ,currentCaptures :: Map Square [Bool]
                       ,captures :: Map Square ([Piece], [Piece])
                       } deriving (Generic, NFData)

currentMove :: MoveSet -> Maybe Move
currentMove ms = join $ fst <$> lookup (currentCaptures ms) (moveMap ms)

makeMoveSet :: Board -> Colour -> [Arrow] -> [Square] -> Maybe MoveSet
makeMoveSet board player as lts
  | null moves = Nothing
  | otherwise = Just $ MoveSet {moveMap = map (first (capturesToFlags allCaps)) moves
                               ,currentCaptures = capturesToFlags allCaps $ fst
                                                  $ minimumBy (comparing (snd . snd)) moves
                               ,captures = allCaps
                               }
  where
    moves = sortMoves player $ matchingMoves board player as lts
    allCaps = classifyCaptures $ map fst moves

setCapture :: Colour -> Square -> Int -> Bool -> MoveSet -> MoveSet
setCapture player sq i b ms@MoveSet{moveMap, currentCaptures, captures}
  = case filter (maybe False ((== b) . (!! i)) . Map.lookup sq . fst) moveMap of
     [] -> ms
     good -> ms{currentCaptures = fst $ minimumBy (comparing (\(flags, (_, val)) -> (diff flags, val))) good}
  where
    diff :: Map Square [Bool] -> Int
    diff cs = sum $ Map.intersectionWith (\bs1 bs2 -> sum $ zipWith (\b1 b2 -> if b1 == b2 then 0 else 1) bs1 bs2)
                                         cs currentCaptures

toggleCapture :: Colour -> Square -> Int -> MoveSet -> MoveSet
toggleCapture player sq i ms = case Map.lookup sq (currentCaptures ms) of
  Nothing -> ms
  Just bs -> setCapture player sq i (not (bs !! i)) ms

----------------------------------------------------------------
