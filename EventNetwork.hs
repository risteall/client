-- -*- Haskell -*-

{-# LANGUAGE LambdaCase, TupleSections, ScopedTypeVariables, NamedFieldPuns, MultiWayIf, PatternGuards, RecursiveDo, DeriveGeneric, DeriveAnyClass, RecordWildCards, StandaloneDeriving, GADTs, DeriveFunctor #-}


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
import Data.AppSettings
import Data.IORef
import System.Process
import Data.Functor.Identity
import System.IO.Unsafe
import Colour

import Env
import Draw
import Base
import Notation hiding (get, set)
import Match
import GameTree
import Misc
import qualified Node
import Sharp
import Settings

bConf :: MomentIO (Behavior Conf)
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
       -> MomentIO (Behavior [Arrow], Behavior (Maybe Arrow), Event ())
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

      f (a, b) | null (fromMaybe [] a) && isNothing (join b) = Nothing
      f _ = Just ()

  arr <- stepper [] $ unionWith const ([] <$ reset) (filterJust (fst <$> e))
  la <- stepper Nothing $ unionWith const (Nothing <$ reset) (filterJust (snd <$> e))
  return (arr, la, filterJust (f <$> e))

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
            | UpdateClock (Colour, Int)
            | UpdateUsed {playerUsed :: Maybe (Colour, Int), gameUsed :: Maybe Int, timeDiff :: Maybe Int}
--            | UpdateUsed (Colour, Int)
  --          | UpdateGameUsed Int
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

drawTree :: GameTree Node.SomeNode -> Map Int [([Int], Double)] -> Behavior (Render ())
drawTree gt offsets = top <$> setColour Nothing [] <*> f (tree gt) []
  where
    top a b = do
      a
      drawNode treeMargin treeMargin []
      b

    f :: Forest Node.SomeNode -> [Int] -> Behavior (Render ())
    f forest ix = fmap sequence_ $ sequenceA $ zipWith g forest [0..]
      where
        (x, y) = getPos ix
        g (Node node forest') n = h <$> setColour (Just node) (ix ++ [n]) <*> f forest' (ix ++ [n])
          where
            h a b = do
              a
              moveTo x y
              let (x2, y2) = getPos (ix ++ [n])
              lineTo x2 y2
              stroke
              drawNode x2 y2 (ix ++ [n])
              b
    getPos [] = (treeMargin, treeMargin)
    getPos ix = (fromJust (lookup ix (fromJust (Map.lookup (length ix) offsets))), treeMargin + treeYGap * fromIntegral (length ix))
    drawNode x y ix = do
      arc x y treeRadius 0 (2 * pi)
      fill
      when (ix == viewPos gt) $ do
        arc x y (treeRadius * 1.8) 0 (2 * pi)
        stroke
    setColour :: Maybe Node.SomeNode -> [Int] -> Behavior (Render ())
    setColour node ix = (>>= flip setSourceColourAlpha alpha) . liftIO <$> colour  -- !!!!!!!!!!!
      where
        colour | ix == currentPos gt = pure (getConf' currentColour)
               | ix == viewPos gt = pure (getConf' viewColour)
               | otherwise = fromMaybe (return c) <$> maybe (pure Nothing) Node.nodeColour node
        c | isPrefixOf ix (currentPos gt) = RGB 0 0 0
          | otherwise = RGB 0 0 0.8
        alpha = if isPrefixOf ix (pathEnd gt) then 1 else 0.5

drawMoves :: GameTree Node.SomeNode -> Double -> Behavior (DrawingArea -> Render ())
drawMoves gt treeWidth = g <$> sequenceA (zipWith f (tail (inits (pathEnd gt))) [0..])
  where
    x = treeWidth + treeMargin
    y n = treeMargin + treeYGap * fromIntegral n
    yText n = y n + treeYGap * 0.85

    f :: [Int] -> Int -> Behavior (Double -> Render (), Double -> Double -> Render (), String, String)
    f ix n = (\col s -> (bg1, bg2 col, moveNum n, s)) <$> Node.nodeColour this <*> Node.movelistEntry this
      where
        Just this = derefNode (tree gt) ix
        bg1 w = do
          let
            f = do
              rectangle x (y n) (w - x) treeYGap
              fill
            c = if | ix == currentPos gt -> currentColour
                   | even n -> goldColour
                   | otherwise -> silverColour
          liftIO (getConf' c) >>= setSourceColour
          f
          when (ix == viewPos gt) $ do
            liftIO (getConf' viewColour) >>= flip setSourceColourAlpha 0.5
            f
        bg2 col x' w = do
          c <- liftIO $ if | Just c <- col -> c
                           | even n -> toSRGB . blend 0.95 black . toColour <$> getConf' goldColour
                           | otherwise -> toSRGB . blend 0.95 black . toColour <$> getConf' silverColour
          setSourceColour c
          rectangle x' (y n) (w - x') treeYGap
          fill

    g :: [(Double -> Render (), Double -> Double -> Render (), String, String)] -> DrawingArea -> Render ()
    g l canvas = do
      w <- liftIO $ fromIntegral <$> widgetGetAllocatedWidth canvas
      setFontSize (treeYGap * 0.75)
      let (bg1s, bg2s, s1s, s2s) = unzip4 l
      mapM_ ($ w) bg1s
      setSourceRGB 0.8 0.8 0.8
      xs <- forM (zip s1s [0..]) $ \(s, n) -> do
        moveTo (x + treeYGap * 0.5) (yText n)
        showText s
        fst <$> getCurrentPoint
      let x' = treeMargin + max (x + 45) (maximum xs)
      mapM_ (\f -> f x' w) bg2s
      setSourceRGB 0.8 0.8 0.8
      forM_ (zip s2s [0..]) $ \(s, n) -> do
        moveTo (x' + treeYGap * 0.5) (yText n)
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

----------------------------------------------------------------

data Stepper a = Stepper
  {initial :: a
  ,event :: Event a
  ,behavior :: Behavior a
  }

mkStepper :: MonadMoment m => a -> Event a -> m (Stepper a)
mkStepper x e = Stepper x e <$> stepper x e

switchStepper :: MonadMoment m => Stepper (Behavior a) -> m (Behavior a)
switchStepper Stepper{initial, event} = switchB initial event

-- deadlocks without laziness, I don't know why
instance Functor Stepper where
  fmap f ~Stepper{initial, event, behavior} = Stepper (f initial) (f <$> event) (f <$> behavior)

----------------------------------------------------------------

-- fails if pure and impure event occur together
treeAccum :: a -> Event (a -> (a, b)) -> Event (a -> MomentIO (a, c)) -> MomentIO (Stepper a, Event b, Event c)
treeAccum init pures impures = mdo
  let q1 = flip ($) <$> b <@> pures
  q2 <- execute ((flip ($)) <$> b <@> impures)
  let e = unionWith const (fst <$> q1) (fst <$> q2)
  b <- stepper init e
  return (Stepper init e b, snd <$> q1, snd <$> q2)

----------------------------------------------------------------

treeNetwork :: Forest Node.SomeNode
            -> [Int]
            -> Event Node.SomeNode
            -> Event (GameTree Node.SomeNode -> MomentIO (GameTree Node.SomeNode, Maybe SharpProcess))
            -> Event ()
            -> Behavior Bool
            -> Behavior Bool
            -> Event ()
            -> Event ()
            -> MomentIO (Stepper (GameTree Node.SomeNode), Event ())
treeNetwork initialTree initialGamePos eMove ePlan eSharp killPlans haveInput eInput eSecond = mdo
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
  eToggleSharp <- fromAddHandler (get toggleSharpAH)

  let eTreeMove = treeMove <$> killPlans <*> haveInput <*> behavior sTree <@> eMove
  let initTree = mkGameTree initialTree initialGamePos (take 2 initialGamePos)

  let
    eInput' = (f <$> behavior sTree) <@ eInput
      where
        f :: GameTree Node.SomeNode -> [SharpProcess]
        f gt
          | Just (Node.SomeNode n) <- viewNode gt
          , Node.CS s <- Node.content n
          = [s]
          | otherwise = []
  
--  (eTree, bTree) <- initAccum initTree
  let pures = unions [(\gt -> select (if null (viewPos gt) then [] else init (viewPos gt)) gt) <$ ePrevNode
                     ,(\gt -> select (take (length (viewPos gt) + 1) (pathEnd gt)) gt) <$ eNextNode
                     ,(\gt -> select (if length (viewPos gt) <= 2 then [] else take 2 (viewPos gt)) gt) <$ eStart
                     ,(\gt -> select (pathEnd gt) gt) <$ eEnd
                     ,(\gt -> select (currentPos gt) gt) <$ eCurrent
                     ,prevBranch <$ ePrevBranch
                     ,nextBranch <$ eNextBranch
                     ,select <$> eSelect
                     ,const . (\(a,_,_) -> a) <$> eTreeMove
--                     ,treePlan <$> ePlan
                     ]

  (sTree, eDel, ePauseAndToggle) <-
    let
      z :: Monad m => (b -> b -> b) -> (a -> m (a, b)) -> (a -> m (a, b)) -> a -> m (a, b)
      z k f g gt = do
        (gt', s1) <- f gt
        (gt'', s2) <- g gt'
        return (gt'', k s1 s2)
      y :: (b -> b -> b) -> (a -> (a, b)) -> (a -> (a, b)) -> a -> (a, b)
      y k f g gt = runIdentity $ z k (Identity . f) (Identity . g) gt

      fromPause e = (fmap (second ((,[]) . maybeToList)) .) <$> e
      fromToggle e = (fmap (second (([],) . maybeToList)) .) <$> e
    in treeAccum initTree
         (foldr (unionWith (y (++))) never
           ((((, []) .) <$> pures)
           : [deleteViewNode <$ eDeleteNode
             ,deleteLine <$ eDeleteLine
             ,deleteAll <$ eDeleteAll
             ,deleteFromHere <$ eDeleteFromHere
             ]))
         (foldr (unionWith (z (\(a1, b1) (a2, b2) -> (a1++a2, b1++b2)))) never
           [fromPause ePlan
           ,fromPause (Node.addSharp (unionWith (++) (fst <$> ePauseAndToggle) eInput') (snd <$> ePauseAndToggle) eSecond <$ eSharp)
           ,fromToggle (Node.toggleSharp <$ eToggleSharp)
           ])

  reactimate $ mapM_ Node.killNode <$> unionWith (++) eDel ((\(_,_,a) -> a) <$> eTreeMove)

  let
    sTreePlaces = (\gt -> (gt, placeTree (tree gt) (currentPos gt))) <$> sTree   -- combined because used together in drawTree
    eSelect = filterJust $ (\(gt, (offsets, width)) -> mouseNode (pathEnd gt) offsets width) <$> behavior sTreePlaces <@> eMouse
    eClear = foldr (unionWith const) never
                   [eStart, eEnd, eCurrent, ePrevNode, eNextNode, ePrevBranch, eNextBranch, eDeleteNode, eDeleteLine
                   ,void eSelect, void ePlan
                   ,void eSharp  -- should be filtered somewhat
                   ,void $ filterE (\(_,a,_) -> a) eTreeMove
                   ,void $ whenE ((\gt -> not (viewPos gt `isPrefixOf` currentPos gt)) <$> behavior sTree) eDeleteAll
                   ]

  bDrawTree <- switchStepper $ (\(gt, (offsets, _)) -> drawTree gt offsets) <$> sTreePlaces
  bDrawMoves <- switchStepper $ (\(gt, (_, width)) -> drawMoves gt width) <$> sTreePlaces

  onChanges $ get setDrawTree <$> ((\x1 x2 canvas -> do {x1; x2 canvas}) <$> bDrawTree <*> bDrawMoves)
      
  reactimate $ (\t -> do
      Gtk.set (get (treeCanvas . widgets))
              [widgetHeightRequest := round (fromIntegral (treeDepth (tree t)) * treeYGap + 2 * treeMargin)]
      a <- scrolledWindowGetVAdjustment (get (treeScrolledWindow . widgets))
      let y = treeMargin + fromIntegral (length (viewPos t)) * treeYGap
      v <- adjustmentGetValue a
      p <- adjustmentGetPageSize a
      when (y < v + 2 * treeYGap) $ Gtk.set a [adjustmentValue := y - 2 * treeYGap]
      when (y > v + p - 2 * treeYGap) $ Gtk.set a [adjustmentValue := y - p + 2 * treeYGap]
    ) <$> event sTree

  return (sTree, eClear)
  
----------------------------------------------------------------

tickFrequency = 10 :: Int

onChanges b = mdo
  valueBLater b >>= liftIOLater
  reactimate' =<< changes b
--onChanges = (>>= reactimate') . changes

----------------------------------------------------------------

--default(String, Int)

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
       -> Event Update
       -> Event ()
       -> MomentIO (Colour -> MomentIO (Behavior Clocks), Behavior String)
clocks params gameState eUpdate eTick = do
  let
    eDiff = filterJust (f <$> eUpdate)
      where f UpdateUsed{timeDiff} = timeDiff
            f _ = Nothing

    -- attempt to account for lag suggested by lightvector in chat
  bMinDiff <- accumB Nothing $ (\d -> Just . maybe d (min d)) <$> eDiff

  let
    isDran player = ((== player) . posToMove . position) <$> gameState
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
    
-- problem?: redundancy between gameState and tree
gameNetwork (params :: GameParams)
            (initialTree :: Forest Node.SomeNode)
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
  eSharp <- fromAddHandler (get sharpAH)

  eBlind <- fromAddHandler (get blindModeAH)
  initialBlindMode <- liftIO $ get getBlindMode
  visible <- fmap (\(seeFriendly, seeEnemy) -> mapColourArray (\c -> if isUser params ! c then seeFriendly else seeEnemy))
                   <$> stepper initialBlindMode eBlind

  eEscape <- fromAddHandler (get clearArrowsAH)

  eCopy <- fromAddHandler (get copyMovelistAH)
  
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
      view :: Behavior (Maybe Node.SomeNode)  -- Nothing for the root
      view = viewNode <$> bTree
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

      nextMove :: Behavior (String, Maybe (GenMove, Position))
      nextMove = f <$> viewPosition <*> ms <*> shadowBoard <*> visible
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
                       Just (x:_) -> Node.regularMove =<< derefNode (tree tp) (currentPos tp ++ [x])
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

      eMoveTime = filterJust (f <$> (posToMove . position <$> gameState)
                                <*> bClocks ! Gold
                                <*> bClocks ! Silver
                                <@> eUpdate)
        where f _ _ _ (UpdateMove (m, Just t)) = Just (m, Just t)
              f c gc sc (UpdateMove (m, Nothing)) = Just (m, g (case c of Gold -> gc; Silver -> sc))
              f _ _ _ _ = Nothing
              g cl | used cl == 0 = Nothing
                   | otherwise = Just $ used cl
  
      -- -- problem: redundant calls to playGenMove
      -- eMove = f <$> (position <$> gameState) <@> eMoveTime
      --                                                        -- !!!!!!!!!!!!!!!!
      --   where f pos (m,x) = either error (\p -> Node.RegularNode (posBoard pos) m p x Nothing) (playGenMove pos m)

      -- not using gameState is probably bad
      eMove = fromMaybe (error "Current node not regular")
          <$> (f <$> ((\gt -> derefNode (tree gt) (currentPos gt)) <$> bTree) <@> eMoveTime)
        where
          f node (move, t) = Node.useRegular' node $ \r -> case playGenMove (Node.regularPosition r) move of
            Left e -> error e
            Right p -> Node.SomeNode $ Node.mkRegularNode r move p ((,0) <$> t)

  viewPosition <- switchStepper $ Node.position . viewNode <$> sTree

----------------------------------------------------------------


  let ePlanFunc = filterJust (b <@ ePlan)
        where b = (\c x v -> if getSetting' c enablePlans
                                  then (\(m,p) -> Node.addFromRegular (\r -> Just (return (Node.SomeNode (Node.mkRegularNode r m p Nothing))))) <$> snd x
                                  else Nothing)
                    <$> bConf' <*> nextMove <*> view

  bTicks <- accumB 0 ((+ 1) <$ eTick)
  let eSecond = whenE ((== 0) . (`mod` tickFrequency) <$> bTicks) eTick

  (sTree, eChangeView)
    <- treeNetwork initialTree
                   initialGamePos
                   eMove
                   ePlanFunc
                   (whenE ((\gs -> not (or (isUser params)) || isJust (result gs)) <$> gameState) eSharp)
                   (flip getSetting' killPlans <$> bConf')
                   haveInput
                   eInput
                   eSecond

  let
    eTree = event sTree
    bTree = behavior sTree

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
  
  (as, la, eInput) <- arrows visible (posBoard <$> viewPosition) eArrowLeft (squareMap <@> eRelease) (squareMap <@> eMotion) eClear

  let emptyLiveTraps = Map.fromList (map (,False) trapSquares)
  liveTraps <- accumB emptyLiveTraps $ unions [const emptyLiveTraps <$ eClear
                                               -- TODO: add left button trap toggle
                                              ,(\sq -> Map.mapWithKey (\trap x -> if trap == sq then not x else x))
                                                   <$> (squareMap <@> eRight)
                                              ]

  let haveInput = (\as la lt s -> not (null as) || isJust la || or lt || not (emptyShadow s)) <$> as <*> la <*> liveTraps <*> shadowBoard
  
  ms <- moveSet (posBoard <$> viewPosition) (Node.toMove <$> view) as liveTraps eToggleCapture

  shadowBoard <- accumB newShadowBoard $ unions $ [const newShadowBoard <$ eClear
                                                  ,flipShadowSquare <$> (Node.toMove <$> view) <@> eSetupToggle
                                                  ] ++ zipWith (\i e -> (\(ShadowBoard b r c)
                                                                            -> ShadowBoard b r (if r ! i == 0 then c else i)) <$ e)
                                                               [0..] (reverse eSetupIcon)

  (makeClocks, gameClockLabel) <- clocks params gameState eUpdate eTick
  onChanges $ labelSetText (get (gameClock . widgets)) <$> gameClockLabel

  bClocks <- (sequenceA $ mapColourArray makeClocks) :: MomentIO (Array Colour (Behavior Clocks))

  -- note assumption that player names do not contain markup
  forM_ [Gold, Silver] $ \c -> do
    let nameString = names params ! c ++ maybe "" (printf " (%d)") (ratings params ! c)
    sequence_ $ zipWith3 (\top bottom s -> onChanges $ (\f s -> labelSetMarkup (if (c == Gold) == f then get (top . widgets) else get (bottom . widgets)) s)
                                                            <$> flipped <*> s)
                         [topPlayer, topClock, topUsedClock]
                         [bottomPlayer, bottomClock, bottomUsedClock]
                         [pure nameString, bigLabel <$> bClocks ! c, usedLabel <$> bClocks ! c]

  drawFB <- switchStepper $ drawNode . viewNode <$> sTree
  let drawB = drawFB <*> shadowBoard <*> ((\as la -> maybe as (:as) la) <$> as <*> la) <*> liveTraps <*> ms <*> visible <*> squareMap <*> bConf'

  onChanges $ get setDrawBoard <$> drawB

  onChanges $ labelSetMarkup (get (moveLabel . widgets)) . fst <$> nextMove

  let f _ True _ = "Starting"
      f _ _ True = "Sending"
      f False _ _ = "Start"
      f _ _ _ = "Send"
    in onChanges $ buttonSetLabel (get (sendButton . buttonSet)) <$> (f <$> (started <$> gameState) <*> startStatus <*> sendStatus)

  onChanges $ buttonSetLabel (get (resignButton . buttonSet)) . (\b -> if b then "Resigning" else "Resign") <$> resignStatus

  let setupLabelsB = (\(ShadowBoard _ remaining _) -> map show $ elems remaining) <$> shadowBoard
  onChanges $ zipWithM_ labelSetText (reverse (get setupLabels)) <$> setupLabelsB

  let f setup c b (ShadowBoard _ _ current) v conf = do
          zipWithM_ ($) (if setup then [widgetShow, widgetHide] else [widgetHide, widgetShow])
                        [get (setupGrid . widgets), get (captureGrid . widgets)]
          if setup then zipWithM_ g (reverse (get setDrawSetupIcons)) [0 .. length pieceInfo - 1]
                   else get setDrawCapture (drawCaptures b v (get icons Map.! getSetting' conf pieceSet))
        where
          g set i = set $ drawSetupIcon (i == current) (get icons Map.! getSetting' conf pieceSet ! (c, i))
    in onChanges $ f <$> (Node.setupPhase <$> view) <*> (Node.toMove <$> view) <*> (posBoard <$> viewPosition) <*> shadowBoard <*> visible <*> bConf'

  let f pos visible | and visible = printf "HarLog: %+.2f" $ harlog $ posBoard pos
                     | otherwise = ""
    in onChanges $ labelSetText (get (harlogLabel . widgets)) <$> (f <$> viewPosition <*> visible)

  let f gs = printf "%s (%s)" (show (timeControl params)) (if rated params then "rated" else "unrated")
             ++ maybe "" (\(c,r) -> printf " | %s won (%s)" (show c) (show r)) (result gs)
    in onChanges $ labelSetText (get (gameLabel . widgets)) <$> (f <$> gameState)

    -- TODO: move number, other notations, multiple moves
  bCopy <- switchStepper $ fmap (maybe "" showGenMove) . Node.getMove . viewNode <$> sTree
  let f s = clipboardGet selectionClipboard >>= flip clipboardSetText s
    in reactimate $ f <$> (bCopy <@ eCopy)


    -- do
  --   c <- clipboardGet selectionClipboard
  --   clipboardSetText c (replicate 50 'X')



----------------------------------------------------------------

newGame (params :: GameParams)
        (initialTree :: Forest Node.SomeNode)
        (request :: Request -> IO a)
        (responses :: MomentIO (Maybe (Event a)))
        (updates :: MomentIO (Behavior GameState, Event Update))
        (cleanup :: IO ())
  = do
  join $ readIORef (get killGameRef)

  mySide <- getConf' viewMySide
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
    killSharps
    cleanup

----------------------------------------------------------------
