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

import Env
import Draw
import Base
import Notation hiding (get, set)
import Match
import GameTree
import Node

username :: Settings.Setting (Maybe String)
username = Settings.Setting "username" Nothing

password :: Settings.Setting (Maybe String)
password = Settings.Setting "password" Nothing

viewMySide = Settings.Setting "view-my-side" False
enablePlans = Settings.Setting "enable-plans" True
killPlans = Settings.Setting "kill-plans" True

settingsPlace = Settings.AutoFromAppName "nosteps"

----------------------------------------------------------------

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
arrows :: Behavior (Array Colour Bool)
       -> Behavior Board
       -> Event Square
       -> Event Square
       -> Event Square
       -> Event ()
       -> MomentIO (Behavior [Arrow], Behavior (Maybe Arrow))
arrows visible board press release motion reset = mdo
  let (ePressNonempty, ePressEmpty) = RB.split $ ((\v b sq -> (if not (and v) || isJust (b ! sq) then Left else Right) sq)
                                                      <$> visible <*> board <@> press)
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
  let m = (\a b c d -> Defer (makeMoveSet a b c d)) <$> board <*> player <*> as <*> ((Map.keys . Map.filter id) <$> lts)
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
            | UpdateClock (Array Colour Int, Int)
            | UpdateUsed (Colour, Int)
            | UpdateGameUsed Int
              deriving Show

data GameState = GameState
  {started :: Bool
  ,position :: Position
  ,result :: Maybe (Colour, Reason)
  }

newGameState = GameState{started = False, position = newPosition, result = Nothing}

updateGameState gs UpdateStart = gs{started = True}
updateGameState gs (UpdateMove (m,_)) = either (error . printf "Illegal move from server (%s)")
                                               (\p -> gs{position = p})
                                               $ playGenMove (position gs) m
updateGameState gs (UpdateResult x) = gs{result = Just x}
updateGameState gs _ = gs

----------------------------------------------------------------


treeMargin = 15 :: Double
treeRadius = 3 :: Double
treeXGap = 10 :: Double
treeYGap = 20 :: Double
treeMaxWidth = 50 :: Double

currentColour, viewColour :: (Double, Double, Double)
currentColour = (0, 0.7, 0)
viewColour = (0.9, 0, 0)

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

drawTree :: GameTree a -> Map Int [([Int], Double)] -> Render ()
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
    setColour ix | ix == currentPos gt = let (r,g,b) = currentColour in setSourceRGBA r g b alpha
                 | ix == viewPos gt = let (r,g,b) = viewColour in setSourceRGBA r g b alpha
                 | isPrefixOf ix (currentPos gt) = setSourceRGBA 0 0 0 alpha
                 | otherwise = setSourceRGBA 0 0 0.5 alpha
      where alpha = if isPrefixOf ix (pathEnd gt) then 1 else 0.5

drawMoves :: GameTree GameTreeNode -> Double -> Array Colour Bool -> DrawingArea -> Render ()
drawMoves gt treeWidth visible canvas = do
  w <- liftIO $ fromIntegral <$> widgetGetAllocatedWidth canvas
  setFontSize (treeYGap * 0.75)
  let
    x = treeWidth + treeMargin
    y n = treeMargin + treeYGap * fromIntegral (n-1)
    yText n = y n + treeYGap * 0.85
    f ix n = (background, s1, s2)
      where
        Just GameTreeNode{..} = derefNode (tree gt) ix
        prev = derefNode (tree gt) (init ix)
        c | even n = Silver
          | otherwise = Gold
        (r,g,b) | ix == viewPos gt = viewColour
                | ix == currentPos gt = currentColour
                | c == Silver = (0.6, 0.6, 0.8)   -- Silver
                | otherwise = (0.9, 0.7, 0)  -- Gold
        s1 = maybe "" showTreeDuration moveTime
        s2 = show (div ((posDepth nodePosition) + 1) 2)
             ++ (if even (posDepth nodePosition) then "s" else "g")
             ++ (if visible ! c then " " ++ either (const "") (sm (board prev) (toMove prev)) move else "")
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
      let (ix, dist) = minimumBy (compare `Function.on` snd)
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

treeNetwork :: Forest GameTreeNode
            -> [Int]
            -> Event GameTreeNode
            -> Event GameTreeNode
            -> Behavior (Array Colour Bool)
            -> Behavior Bool
            -> Behavior Bool
            -> MomentIO (Behavior (GameTree GameTreeNode), Event ())
treeNetwork initialTree initialGamePos eMove ePlan visible killPlans haveInput = mdo
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

  let eTreeMove = treeMove <$> killPlans <*> haveInput <*> bTree <@> eMove
  
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
                      ,treePlan <$> ePlan
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
      
  onChanges $ get setDrawTree <$> ((\t o w v canvas -> do {drawTree t o; drawMoves t w v canvas})
                                      <$> bTree <*> bOffsets <*> bWidth <*> visible)
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

  return (bTree, eClear)
  
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

-- problem?: redundancy between gameState and tree
gameNetwork (params :: GameParams)
            (initialTree :: Forest GameTreeNode)
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
      view :: Behavior (Maybe GameTreeNode)  -- Nothing for the root
      view = (\gt -> derefNode (tree gt) (viewPos gt)) <$> bTree
      setup = (< 2) . depth <$> view
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

      nextMove :: Behavior (String, Maybe (GenMove, Position))
      nextMove = f <$> (mPosition <$> view) <*> ms <*> shadowBoard <*> visible
        where
          f :: Position -> Maybe MoveSet -> ShadowBoard -> Array Colour Bool -> (String, Maybe (GenMove, Position))
          f pos mms sb v
            | posSetupPhase pos && fullShadow sb
              = g $ Left [(sq, piece) | (sq, Just (piece, _)) <- assocs (realiseShadow (posToMove pos) sb)]
            | not (posSetupPhase pos)
            , Just ms <- mms
              = case currentMove ms of
                 Nothing -> ("Move is ambiguous", Nothing)
                 Just m -> g (Right m)
            | otherwise = ("", Nothing)
            where g move = either (\s -> ("<span foreground=\"red\">Illegal move (" ++ s ++ ")</span>", Nothing))
                                  (\pos' -> (either (const "") (\m -> if (v ! posToMove pos) then show m else "[Move hidden]") move,
                                             Just (move, pos')))
                                  $ playGenMove pos move
  
      sendMove :: Behavior (Maybe GenMove)
      sendMove = g <$> (posToMove . position <$> gameState) <*> bTree <*> (fmap fst . snd <$> nextMove) <*> sendStatus
        where
          g _ _ _ True = Nothing
          g c tp n _ | not (isUser params ! c) = Nothing
                     | otherwise = case stripPrefix (currentPos tp) (viewPos tp) of
                       Just [] -> n
                       Just (x:_) -> move <$> derefNode (tree tp) (currentPos tp ++ [x])
                       _ -> Nothing
  
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

      eMoveTime = filterJust (f <$> (posToMove . position <$> gameState) <*> bTimeUsed ! Gold <*> bTimeUsed ! Silver <@> eUpdate)
        where f _ _ _ (UpdateMove (m, Just t)) = Just (m, t)
              f c gu su (UpdateMove (m, Nothing)) = Just (m, case c of Gold -> gu; Silver -> su)
              f _ _ _ _ = Nothing
  
      -- problem: redundant calls to playGenMove
      eMove = f <$> (position <$> gameState) <@> eMoveTime
        where f pos (m,x) = either error (\p -> GameTreeNode m p (Just x) Nothing) (playGenMove pos m)

  ePlanMove <- buttonAction (get (planButton . buttonSet))
                            ePlan
                            $ (\c x -> if Settings.getSetting' c enablePlans
                                         then (\(m,p) -> GameTreeNode m p Nothing Nothing) <$> snd x
                                         else Nothing) <$> bConf' <*> nextMove

  (bTree, eChangeView) <- treeNetwork initialTree
                                      initialGamePos
                                      eMove
                                      ePlanMove
                                      visible
                                      (flip Settings.getSetting' killPlans <$> bConf')
                                      haveInput

  let bStartSend = f <$> gameState <*> sendMove
        where
          f _ (Just m) = Just (Just m)
          f gs _ | not (started gs) = Just Nothing
                 | otherwise = Nothing
  
  eStartSend <- buttonAction (get (sendButton . buttonSet)) eSend bStartSend
  startStatus <- send $ if isUser params ! Gold then RequestStart <$ whenE (not . started <$> gameState) eStartSend
                                                else never
  sendStatus <- send $ RequestMove <$> filterJust eStartSend
  resignStatus <- send $ filterJust $ fmap RequestResign defaultColour <$ eResign

    -- inclusion of eSend here is a hack so that viewPos moves when sending a move
  let eClear = foldr (unionWith const) never [eChangeView, eEscape, eSend]
  
  (as, la) <- arrows visible (board <$> view) eArrowLeft (squareMap <@> eRelease) (squareMap <@> eMotion) eClear

  let emptyLiveTraps = Map.fromList (map (,False) trapSquares)
  liveTraps <- accumB emptyLiveTraps $ unions [const emptyLiveTraps <$ eClear
                                               -- TODO: add left button trap toggle
                                              ,(\sq -> Map.mapWithKey (\trap x -> if trap == sq then not x else x))
                                                   <$> (squareMap <@> eRight)
                                              ]

  let haveInput = (\as la lt s -> not (null as) || isJust la || or lt || not (emptyShadow s)) <$> as <*> la <*> liveTraps <*> shadowBoard

  ms <- moveSet (board <$> view) (toMove <$> view) as liveTraps eToggleCapture

  shadowBoard <- accumB newShadowBoard $ unions $ [const newShadowBoard <$ eClear
                                                  ,flipShadowSquare <$> (toMove <$> view) <@> eSetupToggle
                                                  ] ++ zipWith (\i e -> (\(ShadowBoard b r c)
                                                                            -> ShadowBoard b r (if r ! i == 0 then c else i)) <$ e)
                                                               [0..] (reverse eSetupIcon)

  let eDiff = filterJust (f <$> eUpdate)
        where f (UpdateClock (_, diff)) = Just diff
              f _ = Nothing

  -- bDiff :: Behavior (Maybe (Int, Int))  -- (last diff, min diff)
  bTimeDiff <- accumB Nothing $ (\n -> Just . (n,) . maybe n (min n . snd)) <$> eDiff

    -- attempt to account for lag suggested by lightvector in chat
  let bLag = maybe 0 (uncurry (-)) <$> bTimeDiff

  reactimate' =<< changes (printf "Lag: %d\n" <$> bLag)

  let isDran player = ((== player) . posToMove . position) <$> gameState
      clocksRun = (\gs -> started gs && isNothing (result gs)) <$> gameState

      timeUsed, clockState, timeExtra, clock :: Colour -> MomentIO (Behavior Int)
      timeUsed player = (fmap . fmap) (`div` tickFrequency)
                                      $ accumB 0 $ unions [whenE ((&&) <$> clocksRun <*> isDran player) $ (+1) <$ eTick
                                                          ,const <$> filterJust (f <$> isDran player <@> eUpdate)
                                                          ]
        where
          f True (UpdateMove _) = Just 0
          f _ (UpdateUsed (c,n)) | c == player = Just (tickFrequency * n)
          f _ _ = Nothing

      clockState player = accumB (initialReserve (timeControl params))
                                 $ filterJust (f <$> isDran player <@> eUpdate)
        where
          f True (UpdateMove (_,x)) = updateReserve (timeControl params) <$> x
          f _ (UpdateClock (a,_)) = Just (const (a ! player))
          f _ _ = Nothing
      timeExtra = (fmap . fmap . fmap) (\a -> a + increment (timeControl params) - timeAvailable (timeControl params) a)
                                       clockState

      clock p = liftA2 (liftA3 (\l a b -> timeAvailable (timeControl params) a
                                          - b - l
                                          - if isUser params ! p then 2 else 0)
                               bLag)
                       (clockState p) (timeUsed p)

  bTimeUsed <- colourArray <$> mapM timeUsed [Gold, Silver]
  
  -- note assumption that player names do not contain markup
  forM_ [Gold, Silver] $ \c -> do
    let nameString = names params ! c ++ maybe "" (printf " (%d)") (ratings params ! c)
    cl' <- clock c
    extra <- timeExtra c
    let bigLabel = liftA3 bigClockLabel cl' extra (liftA2 (&&) clocksRun (isDran c))
    sequence_ $ zipWith3 (\top bottom s -> onChanges $ (\f s -> labelSetMarkup (if (c == Gold) == f then get top else get bottom) s)
                                                            <$> flipped <*> s)
                         [topPlayer, topClock, topUsedClock]
                         [bottomPlayer, bottomClock, bottomUsedClock]
                         [pure nameString, bigLabel, ("Move time: " ++) . showClockDuration <$> bTimeUsed ! c]

  let drawB = (\node sb as la lt ms v -> if setupPhase node
                                                 then drawSetup node sb (get icons)
                                                 else drawNonsetup node (maybe as (:as) la) lt ms v (get icons))
                  <$> view <*> shadowBoard <*> as <*> la <*> liveTraps <*> ms <*> visible <*> squareMap
  onChanges $ get setDrawBoard <$> drawB

  onChanges $ labelSetMarkup (get moveLabel) . fst <$> nextMove

  let f _ True _ = "Starting"
      f _ _ True = "Sending"
      f False _ _ = "Start"
      f _ _ _ = "Send"
    in onChanges $ buttonSetLabel (get (sendButton . buttonSet)) <$> (f <$> (started <$> gameState) <*> startStatus <*> sendStatus)

  onChanges $ buttonSetLabel (get (resignButton . buttonSet)) . (\b -> if b then "Resigning" else "Resign") <$> resignStatus

  let setupLabelsB = (\(ShadowBoard _ remaining _) -> map show $ elems remaining) <$> shadowBoard
  onChanges $ zipWithM_ labelSetText (reverse (get setupLabels)) <$> setupLabelsB

  let f setup c b (ShadowBoard _ _ current) v = do
          zipWithM_ ($) (if setup then [widgetShow, widgetHide] else [widgetHide, widgetShow])
                        [get setupGrid, get captureGrid]
          if setup then zipWithM_ g (reverse (get setDrawSetupIcons)) [0 .. length pieceInfo - 1]
                   else get setDrawCapture (drawCaptures b v (get icons))
        where
          g set i = set $ drawSetupIcon (i == current) (get icons ! (c, i))
    in onChanges $ f <$> (setupPhase <$> view) <*> (toMove <$> view) <*> (board <$> view) <*> shadowBoard <*> visible

  let f view visible | and visible = printf "HarLog: %+.2f" $ harlog $ board view
                     | otherwise = ""
    in onChanges $ labelSetText (get harlogLabel) <$> (f <$> view <*> visible)

  let f gs = printf "%s (%s)" (show (timeControl params)) (if rated params then "rated" else "unrated")
             ++ maybe "" (\(c,r) -> printf " | %s won (%s)" (show c) (show r)) (result gs)
    in onChanges $ labelSetText (get gameLabel) <$> (f <$> gameState)

  gameTimeUsed <- accumB 0 $ unions [whenE clocksRun $ (+1) <$ eTick
                                    ,filterJust $ (\case UpdateGameUsed t -> Just (const (t * tickFrequency)); _ -> Nothing) <$> eUpdate
                                    ]
  let f n = case gameLimit (timeControl params) of
        Left 0 -> "No game limit"
        Right 0 -> "No game limit"
        Left l -> showClockDuration $ l - div n tickFrequency
        Right l -> printf "Game limit: %d moves" l
    in onChanges $ labelSetText (get gameClock) <$> (f <$> gameTimeUsed)

----------------------------------------------------------------

newGame (params :: GameParams)
        (initialTree :: Forest GameTreeNode)
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
