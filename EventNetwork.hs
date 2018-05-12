-- -*- Haskell -*-

{-# LANGUAGE LambdaCase, TupleSections, ScopedTypeVariables, NamedFieldPuns, MultiWayIf, PatternGuards, RecursiveDo, DeriveGeneric, DeriveAnyClass, RecordWildCards, StandaloneDeriving #-}

module EventNetwork where

import Control.DeepSeq
import Reactive.Banana hiding (split)
import qualified Reactive.Banana as RB
import Reactive.Banana.Frameworks
import Text.Printf
import Data.Array.IArray
import Data.List
import Data.Maybe
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Control.Monad
import Control.Exception
import Data.Tree hiding (drawTree)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk hiding (get, Arrow, rectangle)
import qualified Graphics.UI.Gtk as Gtk
import qualified Data.Function as Function
import Data.Bifunctor
import Data.Time.Clock
import qualified Data.AppSettings as Settings
import Data.IORef
import Data.Ord
import System.IO.Unsafe

import Env
import Draw
import Base
import Notation
import Match
import GameTree
import qualified Node

timeBits :: Int -> [Int]
timeBits n = [d, h, m, s]
  where
    (m', s) = divMod n 60
    (h', m) = divMod m' 60
    (d, h) = divMod h' 24

showTreeDuration :: Int -> String
showTreeDuration n | n < 0 = show n ++ "s"
showTreeDuration 0 = "0"
showTreeDuration n = concat $ zipWith (\n x -> if n == 0 then "" else show n ++ [x])
                                      (timeBits n) "dhms"

showClockDuration' :: Int -> (String, String)
showClockDuration' n | n < 0 = ("", show n ++ "s")
showClockDuration' n = case timeBits n of
  [d,h,m,s] | d > 0 -> (show d ++ "d" ++ show h ++ "h", t)
            | h > 0 -> (show h ++ "h", t)
            | otherwise -> ("", t)
    where t = printf "%02d:%02d" m s

showClockDuration :: Int -> String
showClockDuration = uncurry (++) . showClockDuration'

----------------------------------------------------------------

bConf :: MomentIO (Behavior Settings.Conf)
bConf = do
  c <- liftIO $ readTVarIO (get conf)
  e <- fromAddHandler (get confAH)
  stepper c e

-- setConf :: Settings.Conf -> IO ()
-- setConf c = join $ atomically $ get setConf' c

----------------------------------------------------------------

-- checkButtonBehavior :: CheckButton -> MomentIO (Behavior Bool)
-- checkButtonBehavior b = do
--   (a, ah) <- liftIO $ do
--     (ah, fire) <- newAddHandler
--     b `on` toggled $ toggleButtonGetActive b >>= fire
--     a <- toggleButtonGetActive b
--     return (a, ah)
--   fromChanges a ah

----------------------------------------------------------------

-- press and motion are filtered to be within the board before this is called; release, not
arrows :: Behavior Bool
       -> Behavior (Array Square [Piece])
       -> Event Square
       -> Event Square
       -> Event Square
       -> Event ()
       -> MomentIO (Behavior [Arrow], Behavior (Maybe Arrow))
arrows allowFromEmpty board press release motion reset = mdo
  let (ePressNonempty, ePressEmpty) = RB.split $ ((\afe b sq -> (if afe || not (null (b ! sq)) then Left else Right) sq)
                                                      <$> allowFromEmpty <*> board <@> press)
      pressFunc, pressEmptyFunc, motionFunc, releaseFunc :: [Arrow] -> Maybe Arrow -> Square -> (Maybe [Arrow], Maybe (Maybe Arrow))

      pressFunc a _ sq = (Just newArrows, Just (if sum (map arrowLength newArrows) < stepsPerMove then Just (sq, sq) else Nothing))
        where newArrows = filter ((/= sq) . fst) a
      pressEmptyFunc a _ sq | null a1 = (Nothing, Nothing)
                            | otherwise = (Just a2, Just (Just (head a1)))
        where (a1, a2) = partition ((== sq) . snd) a
      motionFunc a (Just (p, q)) sq
        | sq /= q && sum (map arrowLength ((p,sq) : a)) <= stepsPerMove
          = (Nothing, Just (Just (p, sq)))
      motionFunc _ _ _ = (Nothing, Nothing)
      releaseFunc a (Just (p,_)) sq
        | inRange boardRange sq && sq /= p && sum (map arrowLength newArrows) <= stepsPerMove
          = (Just newArrows, Just Nothing)
        where newArrows = (p,sq) : a
      releaseFunc a _ _ = (Nothing, Just Nothing)
  
      e = foldr (unionWith const) never [pressFunc <$> arr <*> la <@> ePressNonempty
                                        ,pressEmptyFunc <$> arr <*> la <@> ePressEmpty
                                        ,motionFunc <$> arr <*> la <@> motion
                                        ,releaseFunc <$> arr <*> la <@> release
                                        ]

  arr <- stepper [] $ unionWith const ([] <$ reset) (filterJust (fst <$> e))
  la <- stepper Nothing $ unionWith const (Nothing <$ reset) (filterJust (snd <$> e))
  return (arr, la)

-- verboseKill t = do
--   print t
--   killThread t

forkSeq :: NFData a => Behavior a -> MomentIO (Event a)
forkSeq b = do
  tidVar <- liftIO $ newMVar Nothing
  xVar <- liftIO newEmptyMVar
  
  (reactimate' =<<) . changes . flip fmap b $ \x -> do
    oldTid <- takeMVar tidVar
    newTid <- forkIO $ do
      result <- evaluate $ force x
      tid <- takeMVar tidVar
      tryTakeMVar xVar
      putMVar xVar result
      putMVar tidVar tid
    maybe (return ()) killThread oldTid
    putMVar tidVar $ Just newTid
    
  q <- fromPoll $ do
    a <- takeMVar tidVar
    m <- tryTakeMVar xVar
    putMVar tidVar a
    return m

  c <- changes q
  return $ filterJust $ q <@ c

data Defer a = Defer {undefer :: a} deriving (Generic, NFData)

moveSet :: Behavior Board
        -> Behavior Colour
        -> Behavior [Arrow]
        -> Behavior (Map Square Bool)
        -> Event (Square, Int)
        -> MomentIO (Behavior (Maybe MoveSet))
moveSet board player as lts eToggle = do
  let m = (\a b c d -> Defer (makeMoveSet a b c d)) <$> board <*> player <*> as <*> (trapsToList <$> lts)
  e <- forkSeq m
  c <- changes board
  c' <- changes player
  accumB Nothing $ unions [const Nothing <$ unionWith const (void c) (void c')
                          ,const . undefer <$> e
                          ,(\col (sq, n) -> fmap (toggleCapture col sq n)) <$> player <@> eToggle
                          ]

----------------------------------------------------------------

data Request = RequestStart | RequestMove GenMove | RequestResign Colour

data Update = UpdateStart
            | UpdateMove (GenMove, Maybe Int)
            | UpdateResult (Colour, Reason)
            | UpdateClock (Colour, Int)
            | UpdateUsed {playerUsed :: Maybe (Colour, Int), gameUsed :: Maybe Int, timeDiff :: Maybe Int}
              deriving Show

data GameState = GameState
  {started :: Bool
  ,result :: Maybe (Colour, Reason)
  }

newGameState = GameState{started = False, result = Nothing}

updateGameState gs UpdateStart = gs{started = True}
updateGameState gs (UpdateMove (m,_)) = gs{started = True}   -- formerly was a legality check here
updateGameState gs (UpdateResult x) = gs{result = Just x}
updateGameState gs _ = gs

----------------------------------------------------------------

treeMargin = 15 :: Double
treeRadius = 3 :: Double
treeXGap = 10 :: Double
treeYGap = 20 :: Double
treeMaxWidth = 50 :: Double

placeTree :: Forest a -> [Int] -> (Map Int [([Int], Double)], Double)
placeTree forest currentPos = f [([], forest)] 0 (Map.empty, 0)
  where
    sortNodes :: [([Int], Forest a)] -> [([Int], Forest a)]
    sortNodes = uncurry (++) . partition ((`isPrefixOf` currentPos) . fst)
    f :: [([Int], Forest a)] -> Int -> (Map Int [([Int], Double)], Double) -> (Map Int [([Int], Double)], Double)
    f [] _ x = x
    f nodes n (m, maxWidth) = f (nodes >>= \(ix, forest) -> sortNodes $ zipWith (\t a -> (ix ++ [a], subForest t)) forest [0..])
                                (n+1)
                                (Map.insert n offsets m, max maxWidth (width + treeMargin))
      where
        offsets = zipWith (\ix n -> (ix, n * gap + treeMargin))
                          (map fst nodes)
                          [0..]
        nGaps = fromIntegral (length nodes - 1)
        width = min treeMaxWidth (treeXGap * nGaps)
        gap = if nGaps == 0 then 0 else width / nGaps

drawTree :: GameTree Node.Node -> Map Int [([Int], Double)] -> Render ()
drawTree gt offsets = do
    setColour []
    drawNode treeMargin treeMargin []
    f (tree gt) []
  where
    f :: Forest a -> [Int] -> Render ()
    f forest ix = zipWithM_ g forest [0..]
      where
        (x, y) = getPos ix
        g (Node _ forest') n = do
          setColour (ix ++ [n])
          moveTo x y
          let (x2, y2) = getPos (ix ++ [n])
          lineTo x2 y2
          stroke
          drawNode x2 y2 (ix ++ [n])
          f forest' (ix ++ [n])
    getPos [] = (treeMargin, treeMargin)
    getPos ix = (fromJust (lookup ix (fromJust (Map.lookup (length ix) offsets))), treeMargin + treeYGap * fromIntegral (length ix))
    drawNode x y ix = do
      arc x y treeRadius 0 (2 * pi)
      fill
      when (ix == viewPos gt) $ do
        arc x y (treeRadius * 1.8) 0 (2 * pi)
        stroke
    setColour ix | ix == currentPos gt = let (r,g,b) = getConf currentColour in setSourceRGBA r g b alpha
                 | ix == viewPos gt = let (r,g,b) = getConf viewColour in setSourceRGBA r g b alpha
                 | Just node <- derefNode (tree gt) ix
                 , Node.premovable node = let (r,g,b) = getConf premoveColour in setSourceRGBA r g b alpha
                 | isPrefixOf ix (currentPos gt) = setSourceRGBA 0 0 0 alpha
                 | otherwise = setSourceRGBA 0 0 0.5 alpha
      where alpha = if isPrefixOf ix (pathEnd gt) then 1 else 0.5

drawMoves :: GameTree Node.Node -> Double -> Array Colour Bool -> Notation -> DrawingArea -> Render ()
drawMoves gt treeWidth visible notation canvas = do
  w <- liftIO $ fromIntegral <$> widgetGetAllocatedWidth canvas
  setFontSize (treeYGap * 0.75)
  let
    x = treeWidth + treeMargin
    y n = treeMargin + treeYGap * fromIntegral (n-1)
    yText n = y n + treeYGap * 0.85
    f :: [Int] -> Int -> (Render (), String, String)
    f ix n = (background, s1, s2)
      where
        Just node = derefNode (tree gt) ix
        prev = derefNode (tree gt) (init ix)
        c | even n = Silver
          | otherwise = Gold
        (r,g,b) | ix == viewPos gt = getConf viewColour
                | Node.premovable node = getConf premoveColour
                | ix == currentPos gt = getConf currentColour
                | c == Silver = getConf silverColour
                | otherwise = getConf goldColour
        s1 = maybe "" (showTreeDuration . fst) $ Node.times node
        s2 = show (div (Node.depth (Just node) + 1) 2)
             ++ (if even (Node.depth (Just node)) then "s" else "g")
             ++ (if visible ! c then " " ++ Node.moveString notation node else "")
        background = do
          setSourceRGB r g b
          rectangle x (y n) (w - x) treeYGap
          fill
    (bgs, s1s, s2s) = unzip3 $ zipWith f (tail (inits (pathEnd gt))) [1..]
  sequence_ bgs
  setSourceRGB 0 0 0
  xs <- forM (zip s1s [1..]) $ \(s, n) -> do
    moveTo (x + treeYGap * 0.5) (yText n)
    showText s
    fst <$> getCurrentPoint
  let x' = treeMargin + max (x + 80) (maximum xs)
  forM_ (zip s2s [1..]) $ \(s, n) -> do
    moveTo x' (yText n)
    showText s

mouseNode :: [Int] -> Map Int [([Int], Double)] -> Double -> (Double, Double) -> Maybe [Int]
mouseNode pathEnd offsets width (x,y)
  | x <= width + treeMargin = do
      l <- Map.lookup (round level) offsets
      let (ix, dist) = minimumBy (comparing snd)
                                 $ map (second (\off -> sqrt ((x - off) ^ 2
                                                      + (y - (treeMargin + fromIntegral (round level) * treeYGap)) ^ 2)))
                                       l
      if dist <= 10
        then Just ix
        else Nothing
  | otherwise = let n = ceiling level in
                 if n >= 1 && n <= length pathEnd
                   then Just (take n pathEnd) else Nothing
  where
    level = (y - treeMargin) / treeYGap

treeNetwork :: Array Colour Bool
            -> Forest Node.Node
            -> [Int]
            -> Event Node.Node
            -> Event Node.Node
            -> Event ()
            -> Behavior (Array Colour Bool)
            -> Behavior Bool
            -> Behavior Bool
            -> Behavior Notation
            -> MomentIO (Behavior (GameTree Node.Node), Event GenMove, Event ())
treeNetwork isUser initialTree initialGamePos eMove ePlan eEnablePremoves visible killPlans moveWithCurrent bNotation = mdo
  eStart <- fromAddHandler (get startAH)
  ePrevNode <- fromAddHandler (get prevAH)
  eCurrent <- fromAddHandler (get currentAH)
  eNextNode <- fromAddHandler (get nextAH)
  eEnd <- fromAddHandler (get endAH)
  ePrevBranch <- fromAddHandler (get prevBranchAH)
  eNextBranch <- fromAddHandler (get nextBranchAH)
  eDeleteNode <- fromAddHandler (get deleteNodeAH)
  eDeleteLine <- fromAddHandler (get deleteLineAH)
  eDeleteAll <- fromAddHandler (get deleteAllAH)
  eDeleteFromHere <- fromAddHandler (get deleteFromHereAH)
  eMouse <- fromAddHandler (get treePressAH)

  let f node | Node.premovable node = Node.move node
             | otherwise = Nothing
      getPremove gt = case deref (tree gt) (currentPos gt) of
        (node, subs) | isUser ! Node.toMove node -> listToMaybe $ mapMaybe (f . rootLabel) subs
                     | otherwise -> Nothing
      eTreeMove = (\kp -> treeMove Node.match
                                   (if kp then const Nothing else Node.convert)
                                   ((==) `Function.on` Node.position . Just)
                                   Node.propagate)
                     <$> killPlans <*> moveWithCurrent <*> bTree <@> eMove
      ePremove = filterJust (getPremove . fst <$> eTreeMove)

  bTree <- accumB (mkGameTree initialTree initialGamePos (take 2 initialGamePos))
             $ unions [(\gt -> select (if null (viewPos gt) then [] else init (viewPos gt)) gt) <$ ePrevNode
                      ,(\gt -> select (take (length (viewPos gt) + 1) (pathEnd gt)) gt) <$ eNextNode
                      ,(\gt -> select (if length (viewPos gt) <= 2 then [] else take 2 (viewPos gt)) gt) <$ eStart
                      ,(\gt -> select (pathEnd gt) gt) <$ eEnd
                      ,(\gt -> select (currentPos gt) gt) <$ eCurrent
                      ,prevBranch <$ ePrevBranch
                      ,nextBranch <$ eNextBranch
                      ,select <$> eSelect
                      ,deleteViewNode <$ eDeleteNode
                      ,deleteLine <$ eDeleteLine
                      ,deleteAll <$ eDeleteAll
                      ,deleteFromHere <$ eDeleteFromHere
                      ,const . fst <$> eTreeMove
                      ,mapCurrentToView Node.enablePremove <$ eEnablePremoves
                      ,treePlan Node.planEq <$> ePlan
                      ]
  let
    eClear = foldr (unionWith const) never
                   [eStart, eEnd, eCurrent, ePrevNode, eNextNode, ePrevBranch, eNextBranch, eDeleteNode, eDeleteLine
                   ,void eSelect, void ePlan
                   ,void $ filterE snd eTreeMove
                   ,void $ whenE ((\gt -> not (viewPos gt `isPrefixOf` currentPos gt)) <$> bTree) eDeleteAll
                   ]
    bPlaces = (\gt -> placeTree (tree gt) (currentPos gt)) <$> bTree
    bOffsets = fst <$> bPlaces
    bWidth = snd <$> bPlaces
    eSelect = filterJust (mouseNode <$> (pathEnd <$> bTree) <*> bOffsets <*> bWidth <@> eMouse)
      
  onChanges $ get setDrawTree <$> ((\t o w v n canvas -> do {drawTree t o; drawMoves t w v n canvas})
                                      <$> bTree <*> bOffsets <*> bWidth <*> visible <*> bNotation)
  onChanges $ (\t -> do
      Gtk.set (get treeCanvas)
              [widgetHeightRequest := round (fromIntegral (treeDepth (tree t)) * treeYGap + 2 * treeMargin)]
      a <- scrolledWindowGetVAdjustment (get treeScrolledWindow)
      let y = treeMargin + fromIntegral (length (viewPos t)) * treeYGap
      v <- adjustmentGetValue a
      p <- adjustmentGetPageSize a
      when (y < v + 2 * treeYGap) $ Gtk.set a [adjustmentValue := y - 2 * treeYGap]
      when (y > v + p - 2 * treeYGap) $ Gtk.set a [adjustmentValue := y - p + 2 * treeYGap]
    ) <$> bTree

  return (bTree, ePremove, eClear)
  
----------------------------------------------------------------

tickFrequency = 10 :: Int

onChanges b = mdo
  valueBLater b >>= liftIOLater
  reactimate' =<< changes b
--onChanges = (>>= reactimate') . changes

----------------------------------------------------------------

default(String, Int)

flipSquare :: Square -> Square
flipSquare (x, y) = (boardWidth - 1 - x, boardHeight - 1 - y)

-- per-game constants, as seen by the event-handling function
data GameParams = GameParams
  {names :: Array Colour String
  ,ratings :: Array Colour (Maybe Int)
  ,isUser :: Array Colour Bool
  ,timeControl :: TimeControl
  ,rated :: Bool
  }

buttonAction :: Button -> Event () -> Behavior (Maybe a) -> MomentIO (Event a)
buttonAction b e x = do
  onChanges $ widgetSetSensitive b <$> (isJust <$> x)
  return $ filterJust $ x <@ e

roundTripTimes :: Eq b => Event b -> Event b -> MomentIO (Event NominalDiffTime)
roundTripTimes x y = do
  let f a = do
        t <- getCurrentTime
        return (a, t)
  e <- mapEventIO f x
  pending <- accumB [] $ unions [(:) <$> e
                                ,(\b -> filter ((/= b) . fst)) <$> y
                                ]
  let diffTime t = do
        t' <- getCurrentTime
        return $ diffUTCTime t' t
  mapEventIO diffTime $ filterJust $ (\p b -> snd <$> find ((== b) . fst) p) <$> pending <@> y

bigClockLabel :: Int -> Int -> Bool -> String
bigClockLabel clock extra dran = "<span weight=\"bold\" foreground=\"" ++ colour ++ "\""
                                            ++ (if dran then " background=\"white\"" else "") ++ ">"
                                      ++ "<span size=\"large\">" ++ a1 ++ "</span><span size=\"xx-large\">" ++ a2 ++ "</span></span>"
                                  ++ " + " ++ showClockDuration extra
  where
    (a1, a2) = showClockDuration' clock
    colour | clock <= 10 = "red"
           | clock <= 30 = "orange"
           | otherwise = "blue"

augmentTimeUsed user minDiff (t, diff) | t == 0 = 0
                                       | otherwise = t + fromMaybe 0 (liftA2 (-) diff minDiff)
                                                       + if user then 2 else 0

timeUsed :: GameParams
         -> Colour
         -> Behavior (Maybe Int)
         -> Behavior Bool
         -> Behavior Bool
         -> Event Update
         -> Event ()
         -> MomentIO (Behavior Int)
timeUsed params player bMinDiff clocksRun isDran eUpdate eTick
    = fmap (liftA2 (augmentTimeUsed (isUser params ! player))
                   bMinDiff)
           timeUsed'
  where
    timeUsed' :: MomentIO (Behavior (Int, Maybe Int))
    timeUsed' = (fmap . fmap) (first (`div` tickFrequency))
                              $ accumB (0, Nothing) $ unions [whenE ((&&) <$> clocksRun <*> isDran) $ first (+1) <$ eTick
                                                             ,const <$> filterJust (f <$> isDran <@> eUpdate)
                                                             ]
      where
        f True UpdateMove{} = Just (0, Nothing)
        f _ UpdateUsed{playerUsed = Just (c, t), timeDiff} | c == player = Just (tickFrequency * t, timeDiff)
        f _ _ = Nothing

gameTimeUsed :: GameParams -> Behavior (Maybe Int) -> Behavior Bool -> Event Update -> Event () -> MomentIO (Behavior Int)
gameTimeUsed params bMinDiff clocksRun eUpdate eTick
    = fmap (liftA2 (augmentTimeUsed (or (isUser params)))
                   bMinDiff) gameTimeUsed'
  where
    gameTimeUsed' = (fmap . fmap) (first (`div` tickFrequency))
                                  $ accumB (0, Nothing) $ unions [whenE clocksRun $ first (+1) <$ eTick
                                                                 ,filterJust $ f <$> eUpdate
                                                                 ]
      where
        f UpdateUsed{gameUsed = Just t, timeDiff} = Just (const (t * tickFrequency, timeDiff))
        f _ = Nothing

data Clocks = Clocks
  {state :: Int
  ,used :: Int
  ,bigLabel :: String
  ,usedLabel :: String
  }

mkClocks :: TimeControl -> Bool -> Int -> Int -> Clocks
mkClocks tc run state used
    = Clocks{state, used
            ,bigLabel = bigClockLabel clock extra run
            ,usedLabel = "Move time: " ++ showClockDuration used
            }
  where
    clock = timeAvailable tc state - used
    extra = state + increment tc - timeAvailable tc state

clocks :: GameParams
       -> Behavior GameState
       -> Behavior Colour
       -> Event Update
       -> Event ()
       -> MomentIO (Colour -> MomentIO (Behavior Clocks), Behavior String)
clocks params gameState toMove eUpdate eTick = do
  let
    eDiff = filterJust (f <$> eUpdate)
      where f UpdateUsed{timeDiff} = timeDiff
            f _ = Nothing

    -- attempt to account for lag suggested by lightvector in chat
  bMinDiff <- accumB Nothing $ (\d -> Just . maybe d (min d)) <$> eDiff

  let
    isDran player = (== player) <$> toMove
    clocksRun = (\gs -> started gs && isNothing (result gs)) <$> gameState

    clockState :: Colour -> MomentIO (Behavior Int)
    clockState player = accumB (initialReserve (timeControl params))
                               $ filterJust (f <$> isDran player <@> eUpdate)
      where
        f True (UpdateMove (_,x)) = updateReserve (timeControl params) <$> x
        f _ (UpdateClock (c,t)) | c == player = Just (const t)
        f _ _ = Nothing


    gameLabel n = case gameLimit (timeControl params) of
      Left 0 -> "No game limit"
      Right 0 -> "No game limit"
      Left l -> showClockDuration $ l - n
      Right l -> printf "Game limit: %d moves" l

  gUsed <- gameTimeUsed params bMinDiff clocksRun eUpdate eTick
  
  return (\player -> liftA2 (liftA3 (mkClocks (timeControl params))
                                    (liftA2 (&&) clocksRun (isDran player)))
                            (clockState player)
                            (timeUsed params player bMinDiff clocksRun (isDran player) eUpdate eTick),
          gameLabel <$> gUsed)

----------------------------------------------------------------

emptyLiveTraps = Map.fromList (map (,False) trapSquares)

data Input = InputShadow ShadowBoard | InputArrows [Arrow] (Map Square Bool)

inputShadow (InputShadow s) = s
inputShadow _ = emptyShadow

inputArrows (InputArrows as _) = as
inputArrows _ = []

inputTraps (InputArrows _ ts) = ts
inputTraps _ = emptyLiveTraps

haveInput (InputShadow s) = not (nullShadow s)
haveInput (InputArrows as ts) = (not (null as) || or ts)

trapList = trapsToList . inputTraps

planNode :: Maybe Node.Node -> Input -> Maybe MoveSet -> Array Colour Bool -> Bool -> Notation -> Bool -> (String, Maybe Node.Node)
planNode view input ms visible premove notation needGoodArrows
  | Node.setupPhase view = case fullShadow (inputShadow input) of
    False -> ("", Nothing)
    True -> second (>>= (\pos -> Node.mkMove view m pos Nothing)) $ f m
      where m = Left [(sq, piece) | (sq, Just (piece, _)) <- assocs (realiseShadow (Node.toMove view) (inputShadow input))]
  | otherwise = let
      (s, mp) = case ms of
        Nothing -> ("", Nothing)
        Just ms' -> case currentMove ms' of
          Nothing -> ("Move is ambiguous", Nothing)
          Just m -> second (fmap (m,)) $ f (Right m)
      node | premove = Node.mkPremove view (inputArrows input) (trapList input) mp False needGoodArrows
           | otherwise = mp >>= (\(m,p) -> Node.mkMove view (Right m) p Nothing)
    in (s, node)
  where f move | Left pos <- Node.position view
                 = either (\s -> ("<span foreground=\"red\">Illegal move (" ++ s ++ ")</span>", Nothing))
                          (\pos -> (either (const "")
                                           (\m -> if (visible ! Node.toMove view) then showMove notation (posBoard pos) (Node.toMove view) m else "[Move hidden]")
                                           move,
                                    Just pos))
                          $ playGenMove pos move
               | otherwise = ("", Nothing)

sendAction :: GameTree Node.Node -> Maybe Node.Node -> Bool -> Bool -> Bool -> (Maybe GenMove, Maybe (Maybe Node.Node))
sendAction _ _ _ _ True = (Nothing, Nothing)
sendAction gt node currentIsUser plans _ = case stripPrefix (currentPos gt) (viewPos gt) of
  Just [] -> (if currentIsUser then node >>= Node.move else Nothing, Nothing)
  Just (x:_) -> (if currentIsUser then derefNode (tree gt) (currentPos gt ++ [x]) >>= Node.move else Nothing,
                 Just $ if plans then node else Nothing)
  _ -> (Nothing, Nothing)

----------------------------------------------------------------
    
-- problem?: redundancy between gameState and tree
gameNetwork (params :: GameParams)
            (initialTree :: Forest Node.Node)
            (initialGamePos :: [Int])
            (requestFunc :: Request -> IO a)
            (eResponse :: Maybe (Event a))
            (gameState :: Behavior GameState)   -- gameState and eUpdate passed separately to allow the caller the possibility of
            (eUpdate :: Event Update)           --                        choosing how to handle legality checking
            (flipped :: Behavior Bool)
    = mdo
  bConf' <- bConf
  
  eLeft <- fromAddHandler (get leftPressAH)
  eRight <- fromAddHandler (get rightPressAH)
  eRelease <- fromAddHandler (get releaseAH)
  eMotion <- fromAddHandler (get motionAH)
  eSend <- fromAddHandler (get sendAH)
  ePlan <- fromAddHandler (get planAH)
  eTick <- fromAddHandler (get tickAH)
  eSetupIcon <- mapM fromAddHandler (get setupIconAH)
  eResign <- fromAddHandler (get resignAH)

  eBlind <- fromAddHandler (get blindModeAH)
  initialBlindMode <- liftIO $ get getBlindMode
  visible <- fmap (\(seeFriendly, seeEnemy) -> mapColourArray (\c -> if isUser params ! c then seeFriendly else seeEnemy))
                   <$> stepper initialBlindMode eBlind

  eEscape <- fromAddHandler (get clearArrowsAH)
  
  let defaultColour = find (isUser params !) [Gold, Silver]

  (eRequestSent, fireRequestSent) <- newEvent

  let request x = do
        y <- requestFunc x
        fireRequestSent y
        return y

  times <- case eResponse of
    Nothing -> return never
    Just r -> roundTripTimes eRequestSent r

  reactimate $ print <$> times
  
  let squareMap = (\f -> if f then flipSquare else id) <$> flipped
      view :: Behavior (Maybe Node.Node)  -- Nothing for the root
      view = (\gt -> derefNode (tree gt) (viewPos gt)) <$> bTree
      current = (\gt -> derefNode (tree gt) (currentPos gt)) <$> bTree
      setup = Node.setupPhase <$> view
      (eSetupToggle, eLeft') = RB.split $ (\b x -> if b then Left (fst x) else Right x)
                                               <$> setup <@> (first <$> squareMap <@> eLeft)
      splitLeftButton :: Array Colour Bool
                      -> Maybe MoveSet
                      -> (Square, (Double, Double))
                      -> Either (Square, Int) Square
      splitLeftButton v (Just ms) (sq, x) | Just (m, o) <- Map.lookup sq (captures ms)
                                          , Just i <- smallSquare x
                                          , j <- i - length (filter ((v !) . fst) m)
                                          , j >= 0 && j < length o
                                            = Left (sq, j)
      splitLeftButton _ _ (sq, _) = Right sq
      
      (eToggleCapture, eArrowLeft) = RB.split $ splitLeftButton <$> visible <*> ms <@> eLeft'
  
      send :: Event Request -> MomentIO (Behavior Bool)
      send e = do
        let f m = do
              x <- request m
              return $ if isJust eResponse then Just x else Nothing
            g a (Just b) | a == b = Nothing
            g _ b = b
        e' <- mapEventIO f e
        k <- accumB Nothing $ unions [const <$> e'
                                     ,g <$> fromMaybe never eResponse
                                     ]
        return $ isJust <$> k

      eMoveTime = filterJust (f <$> (Node.toMove <$> current)
                                <*> sequenceA bClocks
                                <@> eUpdate)
        where f :: Colour -> Array Colour Clocks -> Update -> Maybe (GenMove, Maybe Int)
              f _ _ (UpdateMove (m, Just t)) = Just (m, Just t)
              f c cl (UpdateMove (m, Nothing)) = Just (m, g (used (cl ! c)))
              f _ _ _ = Nothing
              g 0 = Nothing
              g n = Just n

        -- problem: redundant calls to playGenMove   (maybe... because of gameState)
      eMove = f <$> bTree <@> eMoveTime
        where f gt (m,x)
                | Left p <- Node.position n
                  = either error (\pos -> fromMaybe (error "bar") $ Node.mkMove n m pos ((,undefined) <$> x)) $ playGenMove p m
                | otherwise = error "foo"
                where n = derefNode (tree gt) (currentPos gt)
  
      premove = (\c gt gs -> premovesEnabled c && isNothing (result gs)
                          && currentPos gt `isPrefixOf` viewPos gt)
                   <$> bConf' <*> bTree <*> gameState
                
      bNotation = (\c -> Settings.getSetting' c notation) <$> bConf'
      plans = plansEnabled <$> bConf'
  
      bPlanNode :: Behavior (String, Maybe Node.Node)
      bPlanNode = planNode <$> view <*> input <*> ms <*> visible <*> premove <*> bNotation <*> ((\gt -> currentPos gt == viewPos gt) <$> bTree)

      bSendAction :: Behavior (Maybe GenMove, Maybe (Maybe Node.Node))
      bSendAction = sendAction <$> bTree
                               <*> (snd <$> bPlanNode)
                               <*> (((isUser params !) . Node.toMove) <$> current)
                               <*> (liftA2 (\p i -> p && haveInput i) plans input)
                               <*> sendStatus

--  reactimate' =<< changes (mapM_ print . snd <$> bPlanNode)
  
  eStartSend <- let
      f gs (Nothing, Nothing) | started gs = Nothing
                              | otherwise = Just $ Left ()
      f _ x = Just $ Right x
    in buttonAction (get (sendButton . buttonSet)) eSend (f <$> gameState <*> bSendAction)

  let (eStart, eSend') = RB.split eStartSend
      eTreeSend = filterJust $ snd <$> eSend'

--  reactimate $ printf "eTreeSend: %s\n" . show <$> eTreeSend
  
  startStatus <- send $ if isUser params ! Gold then RequestStart <$ eStart
                                                else never
  sendStatus <- send $ RequestMove <$> unionWith const (filterJust (fst <$> eSend')) ePremove
  resignStatus <- send $ filterJust $ fmap RequestResign defaultColour <$ eResign
  
  ePlanMove <- buttonAction (get (planButton . buttonSet))
                            ePlan
                            $ (\p (_,n) -> if p then n else Nothing) <$> plans <*> bPlanNode

  (bTree, ePremove, eChangeView)
    <- treeNetwork (isUser params)
                   initialTree
                   initialGamePos
                   eMove
                   (unionWith const ePlanMove (filterJust eTreeSend))
                   (void eTreeSend)
                   visible
                   (flip Settings.getSetting' killPlans <$> bConf')
                   ((\c i -> isUser params ! Node.toMove c || not (haveInput i))
                      <$> current <*> input)
                   bNotation

  let eClear = foldr (unionWith const) never [eChangeView, eEscape]

  (as, la) <- arrows ((\v gt p -> not (and v) || p && currentPos gt /= viewPos gt)
                        <$> visible <*> bTree <*> premove)
                     (Node.longBoard <$> view) eArrowLeft (squareMap <@> eRelease) (squareMap <@> eMotion) eClear

  liveTraps <- accumB emptyLiveTraps $ unions [const emptyLiveTraps <$ eClear
                                               -- TODO: add left button trap toggle
                                              ,(\sq -> Map.mapWithKey (\trap x -> if trap == sq then not x else x))
                                                   <$> (squareMap <@> eRight)
                                              ]

  ms <- moveSet (fromMaybe emptyBoard . Node.board <$> view) (Node.toMove <$> view) (inputArrows <$> input) (inputTraps <$> input) eToggleCapture

  shadowBoard <- accumB emptyShadow $ unions $ [const emptyShadow <$ eClear
                                               ,flipShadowSquare <$> (Node.toMove <$> view) <@> eSetupToggle
                                               ] ++ zipWith (\i e -> (\(ShadowBoard b r c)
                                                                         -> ShadowBoard b r (if r ! i == 0 then c else i)) <$ e)
                                                            [0..] (reverse eSetupIcon)

  let input = (\b s as ts -> if b then InputShadow s else InputArrows as ts)
                <$> setup <*> shadowBoard <*> as <*> liveTraps

  (makeClocks, gameClockLabel) <- clocks params gameState (Node.toMove <$> current) eUpdate eTick
  onChanges $ labelSetText (get gameClock) <$> gameClockLabel

  bClocks <- (sequenceA $ mapColourArray makeClocks) :: MomentIO (Array Colour (Behavior Clocks))

  -- note assumption that player names do not contain markup
  forM_ [Gold, Silver] $ \c -> do
    let nameString = names params ! c ++ maybe "" (printf " (%d)") (ratings params ! c)
    sequence_ $ zipWith3 (\top bottom s -> onChanges $ (\f s -> labelSetMarkup (if (c == Gold) == f then get top else get bottom) s)
                                                            <$> flipped <*> s)
                         [topPlayer, topClock, topUsedClock]
                         [bottomPlayer, bottomClock, bottomUsedClock]
                         [pure nameString, bigLabel <$> bClocks ! c, usedLabel <$> bClocks ! c]

  let drawB = (\node i la ms v -> if Node.setupPhase node
                                    then drawSetup node (inputShadow i) (get icons)
                                    else drawNonsetup node (maybe (inputArrows i) (:inputArrows i) la) (inputTraps i) ms v (get icons))
                  <$> view <*> input <*> la <*> ms <*> visible <*> squareMap
  onChanges $ get setDrawBoard <$> drawB

  onChanges $ labelSetMarkup (get moveLabel) . fst <$> bPlanNode

  let f _ True _ = "Starting"
      f _ _ True = "Sending"
      f False _ _ = "Start"
      f _ _ _ = "Send"
    in onChanges $ buttonSetLabel (get (sendButton . buttonSet)) <$> (f <$> (started <$> gameState) <*> startStatus <*> sendStatus)

  onChanges $ buttonSetLabel (get (resignButton . buttonSet)) . (\b -> if b then "Resigning" else "Resign") <$> resignStatus

  let setupLabelsB = (\(ShadowBoard _ remaining _) -> map show $ elems remaining) <$> (inputShadow <$> input)
  onChanges $ zipWithM_ labelSetText (reverse (get setupLabels)) <$> setupLabelsB

  let f setup c b (ShadowBoard _ _ current) v = do
          zipWithM_ ($) (if setup then [widgetShow, widgetHide] else [widgetHide, widgetShow])
                        [get setupGrid, get captureGrid]
          if setup then zipWithM_ g (reverse (get setDrawSetupIcons)) [0 .. length pieceInfo - 1]
                   else get setDrawCapture (drawCaptures b v (get icons))
        where
          g set i = set $ drawSetupIcon (i == current) (get icons ! (c, i))
    in onChanges $ f <$> (Node.setupPhase <$> view) <*> (Node.toMove <$> view) <*> (fromMaybe emptyBoard . Node.board <$> view) <*> (inputShadow <$> input) <*> visible

  let f view visible | and visible = maybe "" (printf "HarLog: %+.2f" . harlog) $ Node.board view
                     | otherwise = ""
    in onChanges $ labelSetText (get harlogLabel) <$> (f <$> view <*> visible)

  let f gs = printf "%s (%s)" (show (timeControl params)) (if rated params then "rated" else "unrated")
             ++ maybe "" (\(c,r) -> printf " | %s won (%s)" (show c) (show r)) (result gs)
    in onChanges $ labelSetText (get gameLabel) <$> (f <$> gameState)

----------------------------------------------------------------

newGame (params :: GameParams)
        (initialTree :: Forest Node.Node)
        (request :: Request -> IO a)
        (responses :: MomentIO (Maybe (Event a)))
        (updates :: MomentIO (Behavior GameState, Event Update))
        (cleanup :: IO ())
  = do
  join $ readIORef (get killGameRef)

  mySide <- getSetting viewMySide
  let initialSide = if mySide then fromMaybe Gold $ find (isUser params !) [Gold, Silver]
                              else Gold

  network <- compile $ do
    eResponse <- responses
    (gameState, eUpdate) <- updates

    eFlip <- fromAddHandler (get flipAH)
    flipped <- accumB (initialSide == Silver) $ not <$ eFlip

    gameNetwork params initialTree (replicate (leftDepth initialTree) 0)
                request
                eResponse gameState eUpdate
                flipped

  actuate network

  writeIORef (get killGameRef) $ do
    pause network
    cleanup

----------------------------------------------------------------
