module Node where

import qualified Data.Function as Function

import Base

-- type MoveNotation = (Move, Array Notation String)

-- data GameTreeNode =
--   MoveNode {moveMove :: Either [(Square, Piece)] MoveNotation
--            ,movePosition :: Position -- after move
--            ,times :: Maybe (Int, Int) -- move time, reserve after move
--            }
--   | PremoveNode {arrows :: [Arrow]
--                 ,liveTraps :: [Square]
--                 ,premoveMove :: Maybe MoveNotation
--                 ,premovable :: Bool
--                 ,premovePosition :: Either Position (Int, Array Square [Piece])
--                 }

data GameTreeNode = GameTreeNode
  {move :: GenMove
  ,nodePosition :: Position -- after move
  ,moveTime :: Maybe Int -- in seconds
  ,reserve :: Maybe Int -- after move
--  ,moveStrings :: Array Notation String
  }

-- mkGameTree :: GenMove -> Position -> Maybe Int -> Maybe Int -> Board -> Colour -> GameTreeNode
-- mkGameTree m p t r b c = GameTreeNode
--   {move = m
--   ,nodePosition = p
--   ,moveTime = t
--   ,reserve = r
--   ,moveStrings = listArray (minBound, maxBound)
--                            $ map (\n -> either "" (showMove n b c) m)
--                                  $ range (minBound, maxBound)

instance Eq GameTreeNode where
  (==) = (==) `Function.on` nodePosition  -- not quite correct

depth :: Maybe GameTreeNode -> Int
depth = maybe 0 (posDepth . nodePosition)

toMove :: Maybe GameTreeNode -> Colour
toMove n | even (depth n) = Gold
         | otherwise = Silver

setupPhase :: Maybe GameTreeNode -> Bool
setupPhase = (< 2) . depth

board :: Maybe GameTreeNode -> Board
board = maybe emptyBoard (posBoard . nodePosition)

-- used in nextMove only
mPosition :: Maybe GameTreeNode -> Position
mPosition = maybe newPosition nodePosition
