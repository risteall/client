module Base (boardWidth, boardHeight, boardRange, Square, trapSquares, stepsPerMove, Colour(..), flipColour
            ,pieceInfo, nSetupRows, setupRows, Piece, Board, emptyBoard, Move(..), padMove
            ,moveToString, Reason(..), TimeControl(..), parseTimeControl, Arrow
            ,step, pieceToChar, squareToString, parseSetup, parseMove, moveToArrows, arrowLength, playMove, moveToPaths
            ,moveToCaptureSet, stringToBoard, Step(..), legalDrag, singleStepsFrom, dragsFrom, addArrows, appendMoves, nSteps
            ,containsCapture, charToPiece, Direction(..), stringToSquare, updateReserve, readReason, harlog
            ,GenMove, showGenMove, Position(posDepth, posBoard), newPosition, readGenMove, playGenMove, timeAvailable, posToMove
            ,posSetupPhase, charToColour, readColour, colourToServerChar, colourArray, mapColourArray, PaddedStep, destination, dirToChar
            ,moveNum, readMoveNum
            )
  where

import Control.Monad
import Data.Array.IArray
import Data.Maybe
import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List.Split
import Text.Read hiding (step, lift)
import Control.Applicative
import Data.Bifunctor
import Data.Function
import Generics.Deriving (conNameOf)

import GHC.Generics (Generic)
import Control.DeepSeq

boardWidth, boardHeight :: Int
boardWidth = 8
boardHeight = 8

type Square = (Int, Int)

boardRange :: ((Int,Int),(Int,Int))
boardRange = ((0,0), (boardWidth - 1, boardHeight - 1))

trapSquares :: [Square]
trapSquares = [(2,2),(2,5),(5,2),(5,5)]

stepsPerMove = 4 :: Int

data Colour = Gold | Silver deriving (Eq, Enum, Show, Ord, Ix, Generic, NFData)

flipColour :: Colour -> Colour
flipColour Gold = Silver
flipColour Silver = Gold

charToColour c | elem c "wg" = Just Gold
               | elem c "bs" = Just Silver
               | otherwise = Nothing

readColour [c] = charToColour c
readColour _ = Nothing

colourToServerChar Gold = 'w'
colourToServerChar Silver = 'b'

pieceInfo :: [(Char, Int)]
pieceInfo = [('r',8),('c',2),('d',2),('h',2),('m',1),('e',1)]

nSetupRows :: Int
nSetupRows = case divMod (sum (map snd pieceInfo)) boardWidth of
  (n, 0) -> n
  _ -> undefined

setupRows :: Colour -> [Int]
setupRows Gold = [boardHeight - nSetupRows .. boardHeight - 1]
setupRows Silver = [0 .. nSetupRows - 1]

type Piece = (Colour, Int)
type Board = Array Square (Maybe Piece)

emptyBoard = listArray boardRange $ repeat Nothing

harlog :: Board -> Double
harlog board = (f Gold - f Silver) / (g*log(8*16/7/15))
  where
    f c = a + b 
      where
        a = sum $ map (\(_,t) -> z $ length $ filter (\(_,t') -> t'>t) enemy) nonrabbits
        b = g * log (fromIntegral (length rabbits) * fromIntegral (length friendly))
        (friendly, enemy) = partition ((== c) . fst) pieces
        (rabbits, nonrabbits) = partition ((== 0) . snd) friendly
    z 0 = 2 / q
    z n = 1 / (q + fromIntegral n)
    pieces = catMaybes (elems board) -- TODO: replace catMaybes
    q = 1.447530126
    g = 0.6314442034

data Direction = East | North | West | South deriving (Eq, Generic, NFData)

opposite :: Direction -> Direction
opposite East = West
opposite West = East
opposite North = South
opposite South = North

pieceToChar :: Piece -> Char
pieceToChar (player, pieceType) = if player == Gold then toUpper c else c
  where c = fst $ pieceInfo !! pieceType

charToPiece :: Char -> Maybe Piece
charToPiece c = (if isUpper c then Gold else Silver,) <$> findIndex ((== toLower c) . fst) pieceInfo

stringToBoard :: String -> Board
stringToBoard s = emptyBoard // map (second Just) (f (0,0) s)
  where
    f :: Square -> String -> [(Square, Piece)]
    f _ "" = []
    f sq _ | not (inRange boardRange sq) = []
    f sq (' ':s) = munch (next sq) s
    f (x,y) (c:s) | not (isDigit c) = case charToPiece c of
                    Nothing -> f (0,y+1) s
                    Just p -> ((x,y),p) : munch (next (x,y)) s
    f sq s = case first read $ span isDigit s of   -- readsPrec didn't work because of 3e4-type substrings
      (n,s') -> munch (iterate next sq !! n) s'
    munch (0,y) (c:s) | c /= ' ' && not (isDigit c) && isNothing (charToPiece c) = f (0,y) s
    munch sq s = f sq s
    next (x,y) | inRange boardRange (x+1,y) = (x+1,y)
               | otherwise = (0,y+1)

squareToString :: Square -> String
squareToString (x, y) = toEnum (fromEnum 'a' + x) : show (8 - y)

stringToSquare :: String -> Maybe Square
stringToSquare [file, rank] | x <- fromEnum file - fromEnum 'a'
                            , y <- fromEnum '8' - fromEnum rank
                            , inRange boardRange (x, y)
                            = Just (x, y)
stringToSquare _ = Nothing

dirToChar :: Direction -> Char
dirToChar East = 'e'
dirToChar North = 'n'
dirToChar West = 'w'
dirToChar South = 's'

charToDir :: Char -> Maybe Direction
charToDir 'e' = Just East
charToDir 'n' = Just North
charToDir 'w' = Just West
charToDir 's' = Just South
charToDir _ = Nothing

destination :: Square -> Direction -> Maybe Square
destination (x, y) dir = if inRange boardRange d then Just d else Nothing
  where
    d = case dir of
      East -> (x+1, y)
      North -> (x, y-1)
      West -> (x-1, y)
      South -> (x, y+1)

data Step = Step {stepSource :: Square, stepDir :: Direction}

step :: Square -> Direction -> Maybe Step
step sq dir | isJust (destination sq dir) = Just $ Step sq dir
            | otherwise = Nothing

stepDest :: Step -> Square
stepDest (Step sq dir) = fromJust $ destination sq dir

stepReverse :: Step -> Step
stepReverse st = Step (stepDest st) (opposite (stepDir st))

adjacent :: Square -> [Square]
adjacent sq = mapMaybe (destination sq) [East, North, West, South]

frozen :: Board -> Square -> Bool
frozen board sq = case board ! sq of
  Nothing -> False
  Just (player, pieceType) -> any (maybe False (\(pl,pieceType2) -> pl /= player && pieceType2 > pieceType)) a
                              && not (any (maybe False ((== player) . fst)) a)
    where a = map (board !) (adjacent sq)

singleStepsFrom :: [Square] -> Board -> Colour -> [Step]
singleStepsFrom squares board player = do
  sq <- squares
  Just (pl, pieceType) <- [board ! sq] -- hack to get the fail function
  guard (pl == player)
  guard (not (frozen board sq))
  dir <- [East, North, West, South]
  Just st <- [step sq dir]
  let q = stepDest st
  guard (pieceType /= 0 || pl == Gold && dir /= South || pl == Silver && dir /= North)
  guard $ isNothing (board ! q)
  return st

dragsFrom :: [Square] -> Board -> Colour -> [(Step, Step)]
dragsFrom squares board player = do
  sq <- squares
  Just (pl, pieceType) <- [board ! sq]
  guard (pl == player)
  guard (not (frozen board sq))
  dir <- [East, North, West, South]
  Just st <- [step sq dir]
  let sq' = stepDest st
  Just (pl', pieceType') <- [board ! sq']
  guard (pl' /= player)
  guard (pieceType > pieceType')
  let pushes = do
        dir' <- [East, North, West, South]
        Just st' <- [step sq' dir']
        let sq'' = stepDest st'
        guard $ isNothing $ board ! sq''
        return (st', st)
      pulls = do
        dir' <- [East, North, West, South]
        Just st' <- [step sq dir']
        let sq'' = stepDest st'
        guard $ isNothing $ board ! sq''
        return (st', stepReverse st)
  pushes ++ pulls

legalDrag :: Board -> Colour -> Step -> Step -> Bool
legalDrag board player s1@(Step sq1 d1) s2@(Step sq2 d2)
  | Just (p1, t1) <- board ! sq1
  , Just (p2, t2) <- board ! sq2
    = isNothing (board ! stepDest s1)
      && sq1 == stepDest s2
      && if | t1 > t2 -> p1 == player
                      && p2 /= player
                      && not (frozen board sq1)
            | t1 < t2 -> p1 /= player
                      && p2 == player
                      && not (frozen board sq2)
            | otherwise -> False
  | otherwise = False

singleSteps :: Board -> Colour -> [Step]
singleSteps = singleStepsFrom (range boardRange)

drags :: Board -> Colour -> [(Step, Step)]
drags = dragsFrom (range boardRange)

isCapture :: Board -> Square -> Bool
isCapture board sq = case board ! sq of
  Nothing -> False
  Just (player, _) -> not $ any (maybe False ((== player) . fst)) $ map (board !) $ adjacent sq

type PaddedStep = (Piece, Square, Maybe Direction)

paddedStep :: Piece -> Square -> Maybe Direction -> Maybe PaddedStep
paddedStep p sq Nothing = Just (p, sq, Nothing)
paddedStep p sq (Just d) | isJust (destination sq d) = Just (p, sq, (Just d))
                         | otherwise = Nothing

-- NonEmpty
newtype Move = Move [PaddedStep] deriving (Generic, NFData, Eq)

instance Show Move where
  show m = moveToString m

containsCapture :: Move -> Bool
containsCapture (Move l) = any (\(_,_,d) -> isNothing d) l

nSteps :: Move -> Int
nSteps (Move l) = length $ filter (\(_,_,d) -> isJust d) l

appendMoves :: Move -> Move -> Move
appendMoves (Move a) (Move b) = Move (a ++ b)

moveToString :: Move -> String
moveToString (Move l) = intercalate " " $ map f l
  where f (p, sq, d) = pieceToChar p : squareToString sq ++ maybe "x" ((:[]) . dirToChar) d

moveToSteps :: Move -> [Step]
moveToSteps (Move l) = mapMaybe f l
  where
    f (_, sq, Just d) = Just $ Step sq d
    f _ = Nothing

-- HACK
-- TODO: add legality check
playMove :: Board -> Move -> Maybe Board
playMove b m = Just $ fst $ padMove b (moveToSteps m)

-- assumes step is legal
padStep :: Board -> Step -> (Board, Move)
padStep board st@(Step sq dir) = (board' // map (,Nothing) captures,
                                  Move $ (piece, sq, Just dir) : map (\trap -> (fromJust (board' ! trap), trap, Nothing)) captures
                                 )
  where
    dest = stepDest st
    Just piece = board ! sq
    board' = board // [(sq, Nothing), (dest, board ! sq)]
    captures = filter (isCapture board') trapSquares

padMove :: Board -> [Step] -> (Board, Move)
padMove board move = second (foldr appendMoves (Move [])) $ mapAccumL padStep board move

----------------------------------------------------------------

type GenMove = Either [(Square, Piece)] Move

-- playGenMove :: Board -> GenMove -> Board
-- playGenMove b (Left l) = b // map (second Just) l
-- playGenMove b (Right m) = playMove b m

showGenMove :: GenMove -> String
showGenMove (Left setup) = intercalate " " $ map (\(sq, piece) -> pieceToChar piece : squareToString sq) setup
showGenMove (Right move) = show move

data Position = Position {posBoard :: Board, posDepth :: Int, posHistory :: [(Colour, Board)]} deriving Show

instance Eq Position where
  Position b1 n1 h1 == Position b2 n2 h2 = b1 == b2 && h1 == h2 && mod n1 2 == mod n2 2

posToMove :: Position -> Colour
posToMove (Position _ n _) | even n = Gold
                           | otherwise = Silver

posSetupPhase :: Position -> Bool
posSetupPhase = (< 2) . posDepth

newPosition = Position emptyBoard 0 []

-- TODO: full legality check, perhaps with checked and unchecked versions of this function
playGenMove :: Position -> GenMove -> Either String Position
playGenMove (Position b n _) (Left setup)
  | n >= 2 = Left "Setup too late"
  | otherwise = maybe (Left ("Invalid setup: " ++ show setup)) (\b' -> Right $ Position b' (n+1) [])
                      $ (sequence $ accum (\m p -> case m of Just (Just _) -> Nothing; _ -> Just (Just p))
                                          (Just <$> b)
                                          setup)
playGenMove (Position b n history) (Right move)
  | n < 2 = Left "Nonsetup too early"
  | otherwise = case playMove b move of
    Nothing -> Left ("Invalid move: " ++ show move)
    Just b' | b' == b -> Left "Board unchanged"
            | length (filter (\(c, h) -> h == b' && c /= toMove) history) >= 2 -> Left "Repetition"
            | otherwise -> Right (Position b' (n+1) (if containsCapture move then [] else (toMove, b) : history))
  where toMove = if even n then Gold else Silver

readGenMove :: String -> Maybe GenMove
readGenMove s | (c:_):ws' <- ws
               , isDigit c = f ws'
               | otherwise = f ws
  where
    ws = words s
    f :: [String] -> Maybe GenMove
    f ws@(w:_) | length w == 3 = Just $ Left (parseSetup ws)
               | length w == 4 = Just $ Right (parseMove ws)
    f _ = Nothing

----------------------------------------------------------------

genAddArrows :: (b -> a -> Bool) -> [(a, b)] -> [(a, b)] -> [(a, b)]
genAddArrows f a1 a2 = foldl' g a1 a2
  where
    g x (a, b) = case mapAccumL (\found (u, v) -> if f v a then (True, (u, b)) else (found, (u, v))) False x of
      (False, x') -> (a, b) : x'
      (True, x') -> x'

type Arrow = (Square, Square)
--type CaptureArrow = (Square, (Square, Bool{-isCapture-}))

arrowLength :: Arrow -> Int
arrowLength ((a,b),(c,d)) = abs (a-c) + abs (b-d)

-- could remove 0-length arrows
addArrows :: [Arrow] -> [Arrow] -> [Arrow]
addArrows = genAddArrows (==)

--addCaptureArrows :: [CaptureArrow] -> [CaptureArrow] -> [CaptureArrow]
--addCaptureArrows = genAddArrows (\(sq, cap) sq' -> sq == sq' && not cap)

moveToPaths :: Move -> [(Piece, [Square])]
moveToPaths (Move move) = foldr f [] move
  where
    f (_,_,Nothing) l = l
    f (p, sq, Just d) l = case span (\(_,(sq':_)) -> sq' /= dest) l of
        (_, []) -> (p, [sq,dest]) : l
        (l', (_,path):l'') -> (p, sq:path) : l' ++ l''
      where Just dest = destination sq d

-- return value may contain 0-length arrows
moveToArrows :: Move -> Map Piece [Arrow]
moveToArrows = foldl' f Map.empty . moveToPaths
  where
    f m (p, path) = Map.insertWith (++) p [(head path, last path)] m

moveToCaptureSet :: Move -> Map Square [Piece]
moveToCaptureSet (Move move) = foldl' f Map.empty move
  where
    f m (p,sq,Nothing) = Map.alter (Just . maybe [p] (insert p)) sq m
    f m _ = m

----------------------------------------------------------------

data Reason = Goal | Elimination | Immobilization | Timeout | Resignation | IllegalMove | Score | Forfeit deriving Generic

readReason :: String -> Maybe Reason
readReason "g" = Just Goal
readReason "e" = Just Elimination
readReason "m" = Just Immobilization
readReason "t" = Just Timeout
readReason "r" = Just Resignation
readReason "i" = Just IllegalMove
readReason "s" = Just Score
readReason "f" = Just Forfeit
readReason _ = Nothing

instance Show Reason where
  show = dropWhile isSpace . foldr f "" . conNameOf
    where
      f c s | isUpper c = ' ' : toLower c : s
            | otherwise = c : s

-- data TimeControl = forall a. TimeControl
--   {init :: a
--   ,showState :: a -> String
--   ,timeAvailable :: a -> Int
--   ,useTime :: a -> Int -> a
--   ,tcString :: String
--   }

-- instance Show TimeControl where
--   show = tcString



data TimeControl = TimeControl
  { increment :: Int
  , initialReserve :: Int
  , bankingPercentage :: Int
  , reserveCap :: Int
  , gameLimit :: Either Int Int -- Left for seconds, Right for number of moves
  , moveLimit :: Int
  , tcString :: String
  }

instance Show TimeControl where
  show = tcString

maybeMin 0 = id
maybeMin x = min x

updateReserve :: TimeControl -> Int -> Int -> Int
updateReserve tc used reserve
  | excess >= 0 = reserve - excess
  | otherwise = maybeMin (reserveCap tc) (reserve + div (bankingPercentage tc * (- excess)) 100)
  where excess = used - increment tc

timeAvailable :: TimeControl -> Int -> Int
timeAvailable tc reserve = maybeMin (moveLimit tc) (reserve + increment tc)

             -- TODO: 0's
parseTimeControl :: String -> Maybe TimeControl
parseTimeControl s | length l < 2 || length l > 6 = Nothing
                   | otherwise = case take 6 (l ++ repeat "") of
    [m,r,p,l,g,t] -> do
      [m',r',l',t'] <- traverse parseTime [m,r,l,t]
      p' <- if null p then Just 0{- note inconsistency with arimaa.com's description -}
                      else readMaybe p
      g' <- parseGameLimit g
      return $ TimeControl m' r' p' l' g' t' s
  where
    l = splitOn "/" $ filter (not . isSpace) s
    parseTime x | elem ':' x = case sequenceA $ map h $ splitOn ":" x of
                  Just [a,b] -> Just $ 60*a + b
                  Just [a,b,c] -> Just $ 3600*a + 60*b + c
                  _ -> Nothing
                | otherwise = g x 60
    parseGameLimit x | elem ':' x = case sequenceA $ map h $ splitOn ":" x of
                       Just [a,b] -> Just $ Left $ 3600*a + 60*b
                       _ -> Nothing
                     | not (null x) && last x == 't' = fmap Right $ readMaybe (init x)
                     | otherwise = fmap Left $ g x 3600
    g "" _ = Just 0
    g x z = case reads x of
      [(n,"")] -> Just $ n * z
      [(n,suf:rest)] -> liftA2 (\mult r -> n * mult + r) (suffix suf) (g rest z)
      _ -> Nothing
    suffix = flip lookup [('s',1), ('m',60), ('h',3600), ('d',86400)]
    h "" = Just 0
    h x = readMaybe x

----------------------------------------------------------------

parseSetup :: [String] -> [(Square, Piece)]
parseSetup = mapMaybe f
  where
    f (p:sq) = (,) <$> stringToSquare sq <*> charToPiece p
    f _ = Nothing

parseMove :: [String] -> Move
parseMove ss = Move $ mapMaybe f ss
  where
    f [c,f,r,d] = do
      p <- charToPiece c
      sq <- stringToSquare [f,r]
      paddedStep p sq (charToDir d)
    f _ = Nothing

----------------------------------------------------------------

colourArray :: [a] -> Array Colour a
colourArray = listArray (Gold,Silver)

mapColourArray f = listArray (Gold,Silver) $ map f [Gold,Silver]

----------------------------------------------------------------

moveNum :: Int -> String
moveNum n = show (div (n + 2) 2) ++ if even n then "g" else "s"

readMoveNum :: String -> Maybe Int
readMoveNum = f . dropWhile isSpace . dropWhileEnd isSpace
  where
    f "" = Nothing
    f s | last s `elem` "gw" = g =<< readMaybe (init s)
        | last s `elem` "sb" = (+ 1) <$> (g =<< readMaybe (init s))
      where g n | n > 0 = Just (2 * (n - 1))
                | otherwise = Nothing

