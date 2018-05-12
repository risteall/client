{-# LANGUAGE LambdaCase, NamedFieldPuns, RecordWildCards #-}

module Node(Node, mkMove, mkPremove, depth, toMove, setupPhase, board, traps, arrows, move, genBoard, times, premovable, moveString,
           convert, propagate, enablePremove, planEq, position, longBoard, match) where

import Data.Function
import Data.Array.IArray
import Data.Bifunctor
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import System.IO.Unsafe
import Text.Printf

import Base
import Notation
import Match

type MoveNotation = (Move, Array Notation String)

mkMoveNotation :: Board -> Colour -> Move -> MoveNotation
mkMoveNotation b p m = (m, listArray (minBound, maxBound)
                                     $ map (\n -> showMove n b p m) [minBound..maxBound])

-- should MoveNode be premovable ?
data Node =
  MoveNode {moveMove :: Either [(Square, Piece)] MoveNotation
           ,movePosition :: Position -- after move
           ,times_ :: Maybe (Int, Int) -- move time, reserve after move
           }
  | PremoveNode {arrows_ :: [Arrow]
                ,traps_ :: [Square]
                ,premoveMove :: Maybe MoveNotation
                ,premovable_ :: Bool
                ,premovePosition :: Either Position (Int, Array Square [Piece])
                }
  deriving Show

mkMove :: Maybe Node -> Either [(Square, Piece)] Move -> Position -> Maybe (Int, Int) -> Maybe Node
mkMove prev m p t
  | Just b <- board prev = Just MoveNode{moveMove = second (mkMoveNotation b (toMove prev)) m
                                        ,movePosition = p
                                        ,times_ = t
                                        }
  | otherwise = Nothing

type GenPosition = Either Position (Int, Array Square [Piece])

genPosDepth = either posDepth fst

-- returns whether arrows are valid
positionPlusArrows :: GenPosition -> [Arrow] -> (GenPosition, Bool)
positionPlusArrows pos as = (pos', valid)
  where
    b = either (fmap maybeToList . posBoard) snd pos
    f :: (Square, Square) -> (Array Square [Piece], Bool) -> (Array Square [Piece], Bool)
    f (source, dest) (arr, valid) = case b ! source of
      [] -> (arr, False)
      [p] -> (accum (flip (:))
                    (accum (\l x -> case break (== p) l of (a, b) -> a ++ tail b)
                           arr [(source, p)])
                    [(dest, p)],
              valid)
      _ -> (arr, valid)
    (arr', valid) = foldr f (b, True) as
    pos' = case traverse g arr' of
      Nothing -> Right (genPosDepth pos + 1, arr')
      Just b' -> Left $ Position b' (genPosDepth pos + 1) []  -- incorrect treatment of history
    g [] = Just Nothing
    g [p] = Just (Just p)
    g _ = Nothing

-- positionPlusArrows pos as = unsafePerformIO $ do
--   putStrLn "p+a start"
--   printf("pos = %s\n") $ either show (const "") pos
--   printf("as = %s\n") $ show as
--   let x = positionPlusArrows' pos as
--   printf ("return = %s\n") $ either show (const "") $ fst x
--   putStrLn "p+a end"
--   return x

mkPremove :: Maybe Node -> [Arrow] -> [Square] -> Maybe (Move, Position) -> Bool -> Bool -> Maybe Node
mkPremove prev arrows_ traps_ m premovable_ needGoodArrows = case m of
  Just (m', pos) -> Just PremoveNode{arrows_, traps_, premovable_
                                    ,premoveMove = (\bd -> mkMoveNotation bd (toMove prev) m') <$> board prev
                                    ,premovePosition = Left pos
                                    }
  Nothing -> if b || not needGoodArrows   -- does this need to test for empty input ?
               then Just PremoveNode{arrows_, traps_, premovable_
                                    ,premoveMove = Nothing
                                    ,premovePosition = p
                                    }
               else Nothing
    where (p, b) = positionPlusArrows (position prev) arrows_

-- mkPremove :: Maybe Node -> [Arrow] -> [Square] -> Maybe (Move, Position) -> Bool -> Bool -> Maybe Node
-- mkPremove a b c d e f = unsafePerformIO $ do
--   putStrLn "mkPremove start"
--   let x = mkPremove' a b c d e f
--   print x
--   putStrLn "mkPremove end"
--   return x

match :: Node -> Node -> Maybe (Bool, Int)
match moveNode PremoveNode{arrows_, traps_}
  | Just (Right move) <- move moveNode
  , matchArrows arrows_ (concat $ Map.elems $ moveToArrows move)
    && all (`elem` Map.keys (moveToCaptureSet move)) traps_
    = Just $ (False, negate $ length arrows_ + length traps_)
match n1 n2 | ((==) `on` genBoard . Just) n1 n2 = Just (True, 0)
match _ _ = Nothing

position :: Maybe Node -> GenPosition
position (Just n@MoveNode{}) = Left $ movePosition n
position (Just n@PremoveNode{}) = premovePosition n
position Nothing = Left newPosition

planEq :: Node -> Node -> Bool
planEq n1@PremoveNode{} n2@PremoveNode{} = ((==) `on` (\n -> (sort (arrows_ n), sort (traps_ n)))) n1 n2
planEq n1 n2 = ((==) `on` genBoard . Just) n1 n2

-- planEq a b = unsafePerformIO $ do
--   putStrLn "planEq enter"
--   print a
--   print b
--   let x = planEq' a b
--   print x
--   putStrLn "planEq leave"
--   return x

propagate :: Bool -> Node -> Node -> Maybe Node
propagate _ prev node@MoveNode{} = (\pos -> node{movePosition = pos}) <$> mpos
  where mpos = either (\pos -> either (const Nothing) Just $ playGenMove pos (second fst (moveMove node)))
                      (const Nothing)
                      $ position $ Just prev
propagate needGoodArrows prev node
  | validArrows && not (null as && null lts) = case b' of
    Just b | Just move <- makeMoveSet b p as lts >>= currentMove
           , Left pos' <- position $ Just prev
           , Right pos'' <- playGenMove pos' (Right move)
             -> Just node{premoveMove = Just $ mkMoveNotation b p move
                         ,premovePosition = Left pos''
                         }
    _ -> Just node'
  | needGoodArrows = Nothing
  | otherwise = Just node'
  where
    (pos, validArrows) = positionPlusArrows (position $ Just prev) (arrows_ node)
    node' = node{premoveMove = Nothing
                ,premovePosition = pos
                }
    b' = board $ Just prev
    p = toMove $ Just prev
    as = arrows_ node
    lts = traps_ node

convert :: Node -> Maybe Node
convert node@MoveNode{} = Just node
convert PremoveNode{premoveMove = Just m, premovePosition = Left p}
  = Just MoveNode{moveMove = Right m
                 ,movePosition = p
                 ,times_ = Nothing
                 }
convert _ = Nothing

depth :: Maybe Node -> Int
depth = maybe 0 $ \case
  MoveNode{..} -> posDepth movePosition
  PremoveNode{..} -> either posDepth fst premovePosition

toMove :: Maybe Node -> Colour
toMove n | even (depth n) = Gold
         | otherwise = Silver

setupPhase :: Maybe Node -> Bool
setupPhase = (< 2) . depth

genBoard :: Maybe Node -> Either Board (Array Square [Piece])
genBoard = maybe (Left emptyBoard) $ \case
  MoveNode{..} -> Left $ posBoard movePosition
  PremoveNode{..} -> bimap posBoard snd premovePosition

longBoard :: Maybe Node -> Array Square [Piece]
longBoard = either (fmap maybeToList) id . genBoard

board :: Maybe Node -> Maybe Board
board node = either Just (traverse f) $ genBoard node
  where f [] = Just Nothing
        f [x] = Just (Just x)
        f _ = Nothing

traps :: Maybe Node -> [Square]
traps (Just PremoveNode{traps_}) = traps_
traps _ = []

arrows :: Maybe Node -> [Arrow]
arrows (Just PremoveNode{arrows_}) = arrows_
arrows _ = []

move :: Node -> Maybe GenMove
move MoveNode{moveMove} = Just $ second fst moveMove
move PremoveNode{premoveMove} = Right . fst <$> premoveMove

times :: Node -> Maybe (Int, Int)
times MoveNode{times_} = times_
times _ = Nothing

premovable :: Node -> Bool
premovable PremoveNode{premovable_} = premovable_
premovable _ = False

moveString :: Notation -> Node -> String
moveString n MoveNode{moveMove = Right m} = snd m ! n
moveString n PremoveNode{premoveMove = Just m} = snd m ! n
moveString _ _ = ""

enablePremove :: Node -> Node
enablePremove n@PremoveNode{} = n{premovable_ = True}
enablePremove n = n

