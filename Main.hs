-- -*- Haskell -*-

{-# LANGUAGE LambdaCase, TupleSections, ScopedTypeVariables, NamedFieldPuns, MultiWayIf, PatternGuards, RecursiveDo, DeriveGeneric, DeriveAnyClass, RecordWildCards, StandaloneDeriving #-}

{-# LINE 6 "Main.vhs" #-}  --TODO: sub

import Data.Array.IArray
import Graphics.UI.Gtk hiding (get, set, Shift, Arrow, rectangle)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo -- TODO: fix package
import Data.IORef
import Data.Maybe
import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
--import qualified Data.Set as Set
import Data.List.Split
import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent
import Data.Bifunctor
import qualified Data.Function as Function (on)
import Data.Tree hiding (drawTree)
import System.IO.Unsafe
import Text.Read hiding (lift, get)
import Network.HTTP hiding (Request, password)
import Text.Regex
--import qualified Text.Parsec as P hiding ((<|>))

import Control.Monad
import Control.Concurrent.Async

import Control.DeepSeq
import Control.Exception
import GHC.Generics (Generic)

import Reactive.Banana hiding (split)
import qualified Reactive.Banana as RB
import Reactive.Banana.Frameworks

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

import GHC.Exts
import Text.Printf

import Data.Unique
import qualified Data.AppSettings as Settings
import Data.Time.Clock

import Arrows
import Match

import qualified Protocol
import Protocol (arimaaPost, Gameroom, PlayInfo, getFields, GameInfo, reserveSeat, sit)
import Base
import GameTree

#ifndef LOCAL
import Paths_nosteps
#endif

----------------------------------------------------------------

{-
  Want:
    Left click for live traps
    Other notations
    Copy notation
    Move annotations
    Board orientation cue
    Clocks for finished games
    Double-headed arrows
    Premoves
    Chat
-}

{-
TODO:
  check move legality
  check win conditions
  Better layout
  Optimise match, tree
  exception handling
  make move illegal but still draw it when arrowset modified
  logging
  harlog the shown position (after current move)
  grabbing arrows from empty squares doesn't work for inferred arrows
  kill threads on game end
  move recalculated when tree changes, unnecessarily
  key selector: exclude modifiers

  change error calls to something else
-}

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

genDiff :: (a -> b -> Bool) -> [a] -> [b] -> [a]
genDiff pred as bs = foldl' f as bs
  where
    f as b = case break (flip pred b) as of
      (_, []) -> as
      (l1, (_:l2)) -> l1 ++ l2

colourArray :: [a] -> Array Colour a
colourArray = listArray (Gold,Silver)

mapColourArray f = listArray (Gold,Silver) $ map f [Gold,Silver]

----------------------------------------------------------------

username :: Settings.Setting (Maybe String)
username = Settings.Setting "username" Nothing

password :: Settings.Setting (Maybe String)
password = Settings.Setting "password" Nothing

viewMySide = Settings.Setting "view-my-side" False
enablePlans = Settings.Setting "enable-plans" True
killPlans = Settings.Setting "kill-plans" True

settingsPlace = Settings.AutoFromAppName "nosteps"

saveSettings :: IO ()
saveSettings = readTVarIO (get conf) >>= Settings.saveSettings Settings.emptyDefaultConfig settingsPlace

setUsernameAndPassword :: String -> String -> IO ()
setUsernameAndPassword u p = do
  c <- readTVarIO (get conf)
  get setConf $ Settings.setSetting (Settings.setSetting c username (Just u)) password (Just p)
  atomically $ writeTVar (get gameroomRef) Nothing   -- bad: should logout
  getBotLadder

keyBindings :: [(Settings.Setting ([Modifier], KeyVal), String, Maybe (ButtonSet -> Button))]
keyBindings = map (\(a,b,c,d,e) -> (Settings.Setting a (b, keyFromName (fromString c)), d, e))
                  [("send-key", [], "s", "Send move", Just sendButton)
                  ,("resign-key", [], "r", "Resign", Just resignButton)
                  ,("plan-key", [], "space", "Enter plan move", Just planButton)
                  ,("clear-key", [], "Escape", "Clear arrows", Nothing)
                  ,("prev-key", [], "Up", "Previous move", Just prevButton)
                  ,("next-key", [], "Down", "Next move", Just nextButton)
                  ,("start-key", [Gtk.Control], "Up", "Go to game start", Just startButton)
                  ,("end-key", [Gtk.Control], "Down", "Go to game end", Just endButton)
                  ,("current-key", [], "c", "Go to current game position", Just currentButton)
                  ,("prev-branch-key", [], "Left", "Previous variation", Nothing)
                  ,("next-branch-key", [], "Right", "Next variation", Nothing)
                  ,("delete-node-key", [], "BackSpace", "Remove plan move", Just deleteNodeButton)
                  ,("delete-line-key", [Gtk.Control], "BackSpace", "Remove plan variation (back to last branch)", Nothing)
                  ,("delete-all-key", [Gtk.Control, Gtk.Shift], "BackSpace", "Remove all plans", Just deleteAllButton)
                  ,("delete-from-here-key", [], "Delete", "Remove plans starting at current position", Nothing)
                  ]

deriving instance Read Modifier

anyM :: Monad m => [m Bool] -> m Bool
anyM = foldr f (return False)
  where f x y = x >>= \case
          True -> return True
          False -> y

initKeyActions :: Window -> ButtonSet -> IO [AddHandler ()]
initKeyActions w bs = do
  l <- mapM (const newAddHandler) keyBindings
  forM_ (zip l keyBindings) $ \((_, fire), (_, _, mb)) -> forM_ mb $ \b -> b bs `on` buttonActivated $ fire ()
  w `on` keyPressEvent $ do
    k <- eventKeyVal
    m <- eventModifier
    let f (_, fire) (s, _, _) = do
          (m', k') <- getSetting s
          if k == k' && elem m (permutations m')
            then do {fire (); return True}
            else return False
    liftIO $ anyM $ zipWith f l keyBindings
  return $ map fst l

----------------------------------------------------------------
  
entryAccessor :: String -> IO (HBox, IO String, String -> IO ())
entryAccessor label = do
  box <- hBoxNew False 5
  l <- labelNew (Just label)
  e <- entryNew
  entrySetActivatesDefault e True
  containerAdd box l
  containerAdd box e

  return (box, entryGetText e, entrySetText e)

-- class WidgetClass b => WidgetValue a b | a -> b where
--   defaultValue :: a
--   makeWidget :: a -> IO b
--   setValue :: b -> a -> IO ()
--   getValue :: b -> IO (Maybe a)

-- dialogValue :: WidgetValue a b => String -> (a -> IO c) -> IO c
-- dialogValue label f = do
--   d <- newDialog
--   Gtk.set d [windowTransientFor := get window]
--   dialogAddButton d "Cancel" ResponseCancel
--   widgetGrabDefault =<< dialogAddButton d label ResponseOk
--   w <- makeWidget (defaultValue :: a)
--   u <- castToContainer <$> dialogGetContentArea d
--   containerAdd u w
--   widgetShowAll d
--   d `on` response $ \case
--     ResponseOk -> getValue w >>= \case
--       Just x -> do
--         widgetHide d
--         f x
--       Nothing -> return ()
--     _ -> widgetHide d

----------------------------------------------------------------

data ShadowBoard = ShadowBoard Board (Array Int Int) Int

newShadowBoard :: ShadowBoard
newShadowBoard = ShadowBoard emptyBoard (listArray (0, length pieceInfo - 1) (map snd pieceInfo)) (length pieceInfo - 1)

flipShadowSquare :: Colour -> Square -> ShadowBoard -> ShadowBoard
flipShadowSquare c (x,y) sb@(ShadowBoard b remaining current)
    | elem y (setupRows c) = case b ! (x,y) of
      Just (_, pieceType) -> ShadowBoard (b // [((x,y),Nothing)]) (remaining // [(pieceType, remaining ! pieceType + 1)]) pieceType
      Nothing -> ShadowBoard (b // [((x,y), Just (c, current))]) newRemaining (fromMaybe current maybeCurrent)
        where newRemaining = remaining // [(current, remaining ! current - 1)]
              maybeCurrent = find (\n -> newRemaining ! n /= 0) (current : reverse (indices remaining))
    | otherwise = sb

realiseShadow :: Colour -> ShadowBoard -> Array Square (Maybe (Piece, Bool))
realiseShadow c (ShadowBoard sb remaining _) = case map fst $ filter ((/= 0) . snd) (assocs remaining) of
    [n] -> solid // (map (,Just ((c,n), False)) $ filter (\sq -> sb ! sq == Nothing) setupSquares)
    _ -> solid
  where
    solid = fmap (fmap (,True)) sb
    setupSquares = (,) <$> [0 .. boardWidth - 1] <*> setupRows c

fullShadow :: ShadowBoard -> Bool
fullShadow (ShadowBoard _ remaining _) = case filter (/= 0) (elems remaining) of
  _:_:_ -> False
  _ -> True

emptyShadow :: ShadowBoard -> Bool
emptyShadow (ShadowBoard b _ _) = b == emptyBoard

----------------------------------------------------------------

borderWidth = 10 :: Double

squareSize :: WidgetClass w => w -> IO Double
squareSize widget = do
  w <- widgetGetAllocatedWidth widget
  h <- widgetGetAllocatedHeight widget
  let [x,y] = zipWith (\a b -> (fromIntegral a - 2 * borderWidth) / fromIntegral b) [w,h] [boardWidth, boardHeight]
  return $ min x y

boardCoordinates' :: WidgetClass w => w -> (Double, Double) -> IO (Square, (Double, Double))
boardCoordinates' widget (x,y) = do
  s <- squareSize widget
  let [(u,a), (v,b)] = map (properFraction . (/ s) . subtract borderWidth) [x,y]
  return ((u,v), (a,b))

boardCoordinates :: WidgetClass w => w -> (Double, Double) -> IO Square
boardCoordinates widget (x,y) = fst <$> boardCoordinates' widget (x,y)

withTransform :: Double -> Double -> Double -> Render () -> Render ()
withTransform x y size action = do
  save
  translate x y
  scale size size
  action
  restore

drawEmptyBoard :: Render ()
drawEmptyBoard = do
  setSourceRGB 0.9 0.8 0.6
  forM_ trapSquares $ \(x,y) -> do
    moveTo (fromIntegral x) (fromIntegral y)
    relLineTo 0 1
    relLineTo 1 0
    relLineTo 0 (-1)
    closePath
    fill
  setSourceRGB 0.5 0.5 0.5
  (a,b) <- deviceToUserDistance 1 0
  setLineWidth (sqrt (a^2 + b^2))
  forM_ [0..8] $ \x -> do
    moveTo x 0
    lineTo x 8
    stroke
    moveTo 0 x
    lineTo 8 x
    stroke

drawLiveTraps :: Map Square Bool -> (Square -> Square) -> Render ()
drawLiveTraps lt squareMap = do
  setSourceRGB 1 0 0
  setLineWidth 0.1
  forM_ (map squareMap $ Map.keys $ Map.filter id lt) $ \(u, v) -> do
    moveTo (fromIntegral u) (fromIntegral v)
    relLineTo 0 1
    relLineTo 1 0
    relLineTo 0 (-1)
    closePath
    stroke

drawPiece :: Surface -> Double -> Render ()
drawPiece surface alpha = do
  w <- imageSurfaceGetWidth surface
  save
  scale (1 / fromIntegral w) (1 / fromIntegral w)
  setSourceSurface surface 0 0
  paintWithAlpha alpha
  restore

drawPieces :: Array Piece Surface -> Array Colour Bool -> Board -> (Square -> Square) -> Render ()
drawPieces icons visible board squareMap = forM_ (map (first squareMap) (assocs board)) $ \case
  ((x,y), Just piece@(c,_)) | visible ! c -> withTransform (fromIntegral x) (fromIntegral y) 1
                                                           $ drawPiece (icons ! piece) 1
  _ -> return ()

drawSetupPieces :: Array Piece Surface -> Colour -> Board -> ShadowBoard -> (Square -> Square) -> Render ()
drawSetupPieces icons c board shadowBoard squareMap = do
  sequence_ $ zipWith3 (\(x,y) m1 m2 -> maybe (return ())
                                              (withTransform (fromIntegral x) (fromIntegral y) 1)
                                              (m1 <|> m2))
                       (map squareMap (range boardRange))
                       (map (fmap (\p -> drawPiece (icons ! p) 1)) (elems board))
                       (map (fmap (\(p, solid) -> drawPiece (icons ! p) (if solid then 1 else 0.5)))
                            (elems (realiseShadow c shadowBoard)))

trappedPieceSize = 0.4 :: Double

trappedPieceSquares :: [(Double, Double)]
trappedPieceSquares = [(0,0), (x,0), (0,x), (x,x)]
  where x = 1 - trappedPieceSize

smallSquare :: (Double, Double) -> Maybe Int
smallSquare (x,y) = findIndex (\(u,v) -> x>=u && x<=u+trappedPieceSize && y>=v && y<=v+trappedPieceSize)
                              trappedPieceSquares

drawTrappedPiece :: Surface -> Square -> Int -> Maybe Bool -> Render ()
drawTrappedPiece surface (x,y) n question
  = when (inRange (0,3) n) $ withTransform (fromIntegral x + u) (fromIntegral y + v) trappedPieceSize $ do
    drawPiece surface (case question of Just False -> 0.5; _ -> 1)
    case question of
      Nothing -> return ()
      Just q -> do
        if q then setSourceRGB 0.9 0 0 else setSourceRGB 0 0.7 0
        te <- textExtents "?"
        save
        translate 0.1 0.1
        let x = 0.4 / textExtentsWidth te
        scale x x
        moveTo (- textExtentsXbearing te) (- textExtentsYbearing te)
        showText "?"
        restore
  where (u,v) = trappedPieceSquares !! n

drawNonsetup (node :: Maybe GameTreeNode)
             (arrows :: [Arrow])
             (liveTraps :: Map Square Bool)
             (ms :: Maybe MoveSet)
             (visible :: Array Colour Bool)
             (icons :: Array Piece Surface)
             (squareMap :: Square -> Square)
             (canvas :: DrawingArea)
    = do
  x <- liftIO $ squareSize canvas
  setSourceRGB 1 1 1
  paint
  translate borderWidth borderWidth
  scale x x

  drawEmptyBoard
  drawLiveTraps liveTraps squareMap

  drawPieces icons
             (if depth node <= 2 && null arrows then listArray (Gold,Silver) (repeat True) else visible)
             (fromMaybe (board node) (ms >>= currentMove >>= playMove (board node)))
             squareMap

  let pathColour (Just (c,_)) _ True | not (visible ! c) = setSourceRGB 0 0.9 0
      pathColour Nothing _ True | not (visible ! Gold) || not (visible ! Silver) = setSourceRGB 0 0.9 0
      pathColour (Just (Gold,_)) solid _ = setSourceRGBA 1 0 0 (if solid then 1 else 0.5)
      pathColour (Just (Silver,_)) solid _ = setSourceRGBA 0 0 1 (if solid then 1 else 0.5)
      pathColour Nothing _ _ = setSourceRGB 0 0 0

      noInput = null arrows && not (or liveTraps)

  if | depth node <= 2 && noInput -> return ()
     | noInput -> do
         let Right m = move (fromJust node)
             (straight, bendy) =  partition (straightPath . snd) $ moveToPaths m
         drawArrowSet $ map (first (bimap squareMap squareMap))
                            $ map (\(p, path) -> (pathToArrow path, pathColour (Just p) False False))
                                  straight
         forM_ bendy $ \(p, path) -> do {pathColour (Just p) False False; drawPath $ map squareMap path}
         
         forM_ (Map.assocs (moveToCaptureSet m))
               $ \(sq, pieces) -> zipWithM_ (\p i -> drawTrappedPiece (icons ! p) (squareMap sq) i Nothing) pieces [0..]

     | otherwise -> do
         let (straight, bendy) = case ms >>= currentMove of
               Nothing -> ([], [])
               Just m -> partition (straightPath . snd) $ filter (\((c,_),_) -> visible ! c)
                                                        $ genDiff (\(_,p) a -> pathToArrow p == a)
                                                                  (moveToPaths m)
                                                                  arrows
             straightActions = map (\(p, path) -> (pathToArrow path, pathColour (Just p) False False)) straight
             arrActions = map (\a -> (a, pathColour ((board node) ! fst a) True True)) arrows
         drawArrowSet $ map (first (bimap squareMap squareMap)) $ arrActions ++ straightActions
         forM_ bendy $ \(p, path) -> do {pathColour (Just p) False False; drawPath $ map squareMap path}

         -- should make the correspondence between this and buttonPressCallback explicit
         let f :: Square -> [Bool] -> ([Piece], [Piece]) -> Render ()
             f trap bs (mandatory, optional) = zipWithM_ ($) (map g (filter ((visible !) . fst) mandatory)
                                                                ++ zipWith h bs optional)
                                                             [0..]
               where
                 g m i = drawTrappedPiece (icons ! m) (squareMap trap) i Nothing
                 h b o i = drawTrappedPiece (icons ! o) (squareMap trap) i (Just b)

         case ms of
           Nothing -> return ()
           Just MoveSet{currentCaptures, captures} -> sequence_ $ Map.intersectionWithKey f currentCaptures captures

drawSetup :: Maybe GameTreeNode -> ShadowBoard -> Array Piece Surface -> (Square -> Square) -> DrawingArea -> Render ()
drawSetup node sb icons squareMap canvas = do
  x <- liftIO $ squareSize canvas
  setSourceRGB 1 1 1
  paint
  translate borderWidth borderWidth
  scale x x

  drawEmptyBoard

  drawSetupPieces icons (toMove node) (board node) sb squareMap

drawSetupIcon :: Bool -> Surface -> DrawingArea -> Render ()
drawSetupIcon b s da = do
  x <- fromIntegral <$> liftIO (min <$> widgetGetAllocatedWidth da <*> widgetGetAllocatedHeight da)
  scale x x
  let borderWidth = 0.1
  when b $ do
    setLineWidth borderWidth
    moveTo (borderWidth/2) (borderWidth/2)
    lineTo (borderWidth/2) (1 - borderWidth/2)
    lineTo (1 - borderWidth/2) (1 - borderWidth/2)
    lineTo (1 - borderWidth/2) (borderWidth/2)
    closePath
    stroke
  translate borderWidth borderWidth
  z <- ((* (1 - 2*borderWidth)) . recip . fromIntegral) <$> imageSurfaceGetWidth s
  scale z z
  setSourceSurface s 0 0
  paint

drawCaptures :: Board -> Array Colour Bool -> Array Piece Surface -> DrawingArea -> Render ()
drawCaptures board visible icons canvas = do
  w <- liftIO $ widgetGetAllocatedWidth canvas
  h <- liftIO $ widgetGetAllocatedHeight canvas

  let l = concat $ reverse $ zipWith replicate (map snd pieceInfo) [0..]
      pieces = catMaybes (elems board)                              -- TODO: replace catMaybes
      captures c = map (c,) l \\ filter ((== c) . fst) pieces
      n = fromIntegral $ maximum (0 : [length (captures c) | c <- [Gold, Silver], visible ! c])
      z = fromIntegral h / 2
      overlap = if n <= 1 then 0 else max (z*0.1) ((n*z - fromIntegral w) / (n - 1))
  
  forM_ [(Gold, 0), (Silver, z)] $ \(c, y) ->
    when (visible ! c) $ zipWithM_ (\p i -> withTransform (i * (z-overlap)) y z $ drawPiece (icons ! p) 1)
                                   (captures c) [0..]

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
            | UpdateClock (Colour, Int)
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

data GameTreeNode = GameTreeNode {move :: GenMove
                                 ,nodePosition :: Position -- after move
                                 ,moveTime :: Maybe Int -- in seconds
                                 ,reserve :: Maybe Int -- after move
                                 }

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

mPosition :: Maybe GameTreeNode -> Position
mPosition = maybe newPosition nodePosition

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
        c | even n = Silver
          | otherwise = Gold
        (r,g,b) | ix == viewPos gt = viewColour
                | ix == currentPos gt = currentColour
                | c == Silver = (0.6, 0.6, 0.8)   -- Silver
                | otherwise = (0.9, 0.7, 0)  -- Gold
        s1 = maybe "" showTreeDuration moveTime
        s2 = show (div ((posDepth nodePosition) + 1) 2)
             ++ (if even (posDepth nodePosition) then "s" else "g")
             ++ (if visible ! c then " " ++ showGenMove move else "")
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

data ButtonSet = ButtonSet
  {sendButton, planButton, resignButton
  ,startButton, prevButton, currentButton, nextButton, endButton
  ,deleteNodeButton, deleteAllButton :: Button
  }

data Env = Env {icons :: Array (Colour, Int) Surface
               ,moveLabel :: Label
               ,topClock :: Label
               ,bottomClock :: Label
               ,setupGrid :: Grid
               ,setupLabels :: [Label]
               ,captureGrid :: Grid
               ,harlogLabel :: Label
               ,leftPressAH :: AddHandler (Square, (Double, Double))
               ,rightPressAH :: AddHandler Square
               ,releaseAH :: AddHandler Square
               ,motionAH :: AddHandler Square
               ,flipAH :: AddHandler ()
               ,tickAH :: AddHandler ()
               ,setupIconAH :: [AddHandler ()]
               ,killGameRef :: IORef (IO ())
               ,treeCanvas :: DrawingArea
               ,setDrawBoard :: (DrawingArea -> Render ()) -> IO ()
               ,setDrawSetupIcons :: [(DrawingArea -> Render ()) -> IO ()]
               ,setDrawCapture :: (DrawingArea -> Render ()) -> IO ()
               ,setDrawTree :: (DrawingArea -> Render ()) -> IO ()
               ,topPlayer :: Label
               ,bottomPlayer :: Label
               ,window :: Window
               ,gameLabel :: Label
               ,gameClock :: Label
               ,topUsedClock, bottomUsedClock :: Label
               ,blindModeAH :: AddHandler (Bool, Bool)
               ,botLadderBotsRef :: TVar (IO [BotLadderBot])
               ,statusStack :: TVar [(Unique, String)]
               ,statusLabel :: Label
               ,myGames :: TVar [Protocol.GameInfo]
               ,openGames :: TVar [Protocol.GameInfo]
               ,liveGames :: TVar [LiveGameInfo]
               ,postalGames :: TVar [LiveGameInfo]
               ,conf :: TVar Settings.Conf
               ,gameroomRef :: TVar (Maybe Gameroom)
               ,usernameEntry, passwordEntry :: Entry
               ,goldSideButton, mySideButton :: RadioButton
               ,settingsDialog :: Dialog
               ,treePressAH :: AddHandler (Double, Double)
               ,treeScrolledWindow :: ScrolledWindow
               ,getBlindMode :: IO (Bool, Bool)
               ,actionColumn :: TreeViewColumn
               ,accelColumn :: TreeViewColumn
               ,keyTreeView :: TreeView
               ,sendAH :: AddHandler ()
               ,resignAH :: AddHandler ()
               ,planAH :: AddHandler ()
               ,clearArrowsAH :: AddHandler ()
               ,prevAH :: AddHandler ()
               ,nextAH :: AddHandler ()
               ,startAH :: AddHandler ()
               ,endAH :: AddHandler ()
               ,currentAH :: AddHandler ()
               ,deleteNodeAH :: AddHandler ()
               ,deleteLineAH :: AddHandler ()
               ,deleteAllAH :: AddHandler ()
               ,prevBranchAH :: AddHandler ()
               ,nextBranchAH :: AddHandler ()
               ,deleteFromHereAH :: AddHandler ()
               ,buttonSet :: ButtonSet
               ,enablePlansButton, killPlansButton :: CheckButton
               ,setConf :: Settings.Conf -> IO ()
               ,confAH :: AddHandler Settings.Conf
               }

globalEnv :: IORef Env
globalEnv = unsafePerformIO $ newIORef undefined

get :: (Env -> a) -> a
get f = unsafePerformIO $ f <$> readIORef globalEnv

getSetting :: Read a => Settings.Setting a -> IO a
getSetting s = do
  c <- readTVarIO (get conf)
  return $ Settings.getSetting' c s

botLadderBots :: IO [BotLadderBot]
botLadderBots = join $ readTVarIO (get botLadderBotsRef)

gameroom :: IO Gameroom
gameroom = readTVarIO (get gameroomRef) >>= \case
  Just g -> return g
  Nothing -> do
    u <- getSetting username
    p <- getSetting password
    case (u, p) of
      (Just u'@(_:_), Just p'@(_:_)) -> Protocol.login u' p'
      _ -> error "Can't get gameroom"

bConf :: MomentIO (Behavior Settings.Conf)
bConf = do
  c <- liftIO $ readTVarIO (get conf)
  e <- fromAddHandler (get confAH)
  stepper c e

-- setConf :: Settings.Conf -> IO ()
-- setConf c = join $ atomically $ get setConf' c

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
                     | otherwise = case dropPrefix (currentPos tp) (viewPos tp) of
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
          f _ (UpdateClock (c,n)) | c == player = Just (const n)
          f _ _ = Nothing
      timeExtra = (fmap . fmap . fmap) (\a -> a + increment (timeControl params) - timeAvailable (timeControl params) a)
                                       clockState
      clock = (liftA2 . liftA2 . liftA2) (\a b -> timeAvailable (timeControl params) a - b)
                                         clockState timeUsed

  bTimeUsed <- colourArray <$> mapM timeUsed [Gold, Silver]
  
  -- note assumption that player names do not contain markup
  forM_ [Gold, Silver] $ \c -> do
    let nameString = names params ! c ++ maybe "" (printf " (%d)") (ratings params ! c)
--    used <- timeUsed c
    cl <- clock c
    let cl' = if isJust eResponse && isUser params ! c then subtract 2 <$> cl else cl  -- attempt to account for lag
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

background :: IO a -> (a -> IO ()) -> IO ()
background x y = void $ forkIO $ x >>= postGUIAsync . y

showStatus :: IO ()
showStatus = do
  readTVarIO (get statusStack) >>= \case
    [] -> labelSetText (get statusLabel) ""
    (_,m):_ -> labelSetText (get statusLabel) m

setStatus :: String -> Maybe Int -> IO (IO ())
setStatus message timeout = do
  u <- newUnique
  atomically $ modifyTVar (get statusStack) $ ((u, message) :)
  postGUIAsync showStatus
  let remove = do
        atomically $ modifyTVar (get statusStack) $ filter ((/= u) . fst)
        postGUIAsync showStatus
  forM_ timeout $ \n -> void $ forkIO $ do
    threadDelay (n * 1000000)
    remove
  return remove

withStatus :: String -> IO a -> IO a
withStatus message action = do
  remove <- setStatus message Nothing
  finally action remove

sendMessage :: TChan (a, Int) -> TVar Int -> a -> IO Int
sendMessage ch r m = atomically $ do
  n <- readTVar r
  writeTChan ch (m, n)
  writeTVar r (n+1)
  return n

-- parseMoves :: Colour -> String -> (Colour, [(Colour, Message)])
-- parseMoves toMove moveString = (toMove', catMaybes l)
--   where
--     parseMoveNum s = case reads s :: [(Int,String)] of
--       [(_,"w")] -> Just Gold
--       [(_,"b")] -> Just Silver
--       _ -> Nothing
--     f :: Colour -> [String] -> (Colour, Maybe Message)
--     f _ (w:ws) | Just c <- parseMoveNum w
--                  = (c, g ws)
--     f c ws = (c, g ws)
--     g ws@(w:_) | length w == 3 = Just $ SendMove $ Left (parseSetup ws)
--                | length w == 4 = Just $ SendMove $ Right (parseMove ws)
--     g _ = Nothing
--     f' c move = case f c move of
--       (c', m) -> (c', (c',) <$> m)
--     (toMove', l) = mapAccumL f' toMove $ map words $ splitOn "\DC3" moveString

alert :: String -> IO ()
alert s = do
  x <- newEmptyMVar
  postGUIAsync $ do
    d <- messageDialogNew (Just (get window)) [] MessageError ButtonsOk s
    widgetShowAll d
    d `on` response $ \_ -> do
      widgetDestroy d
      putMVar x ()
    return ()
  takeMVar x

toServer :: PlayInfo -> String -> TChan (Request, Int) -> TChan Int -> IO ()
toServer (gsurl, sid) auth requestChan responseChan = forever $ try f >>= \case
    Left (Protocol.ServerError e) -> alert e
    Right _ -> return ()
  where
    f = bracket
          (atomically (readTChan requestChan))
          (\(_, q) -> atomically (writeTChan responseChan q))
          (\(request, _) -> case request of     
            RequestStart -> void $ arimaaPost gsurl [("action", "startgame"), ("sid", sid), ("auth", auth)]
            RequestMove move -> void $ arimaaPost gsurl [("action", "move"), ("sid", sid), ("auth", auth), ("move", showGenMove move)]
            RequestResign _ -> void $ arimaaPost gsurl [("action", "resign"), ("sid", sid), ("auth", auth)]
          )

getUpdates :: [(String, String)] -> Bool -> TChan Update -> IO Bool
getUpdates response started updateChan = do
  let send = atomically . writeTChan updateChan
  
  let start = not started && isJust (lookup "starttime" response)
  when start $ send UpdateStart

  forM_ (lookup "moves" response) $ \m -> let moves = mapMaybe readGenMove $ splitOn "\DC3" m in
    when (not (null moves)) $ mapM_ (send . UpdateMove)
                                    $ map (,Nothing) (init moves) ++ [(last moves, read <$> lookup "lastmoveused" response)]

  forM_ [("tcwreserve2", Gold), ("tcbreserve2", Silver)] $ \(s, c) ->
    forM_ (lookup s response) (send . UpdateClock . (c,) . read)

  case lookup "result" response of
    Just [c1,c2] | Just r <- readReason c2 -> send $ UpdateResult $ (if elem c1 "wg" then Gold else Silver, r)
    _ -> return ()

  return (started || start)

fromServer :: PlayInfo -> [(String, String)] -> Bool -> TChan Update -> IO ()
fromServer (gsurl, sid) response started updateChan = do
  let [lc, ml, cl] = getFields ["lastchange", "moveslength", "chatlength"] response
  
  response' <- handle (\(Protocol.ServerError s) -> do {alert s; return response})
                 $ arimaaPost gsurl [("action", "updategamestate")
                                    ,("sid", sid)
                                    ,("wait", "1")
                                    ,("lastchange", lc)
                                    ,("moveslength", ml)
                                    ,("chatlength", cl)
                                    ]

  started <- getUpdates response' started updateChan
  fromServer (gsurl, sid) response' started updateChan

channelEvent :: TChan a -> MomentIO (Event a)
channelEvent chan = do
  ah <- liftIO $ do
    (ah, fire) <- newAddHandler
    let f = atomically (tryReadTChan chan) >>= maybe (return ()) (\x -> do {fire x; f})
    timeoutAdd (True <$ f) 100
    return ah
  fromAddHandler ah

setServerGame :: GameInfo -> IO ()
setServerGame gameInfo = handle (\(Protocol.ServerError s) -> alert s) $ do
  gameroom <- gameroom
  ri <- reserveSeat gameroom gameInfo
  ((gsurl, sid), _, _) <- sit gameroom ri

  requestChan <- newTChanIO
  responseChan <- newTChanIO
  updateChan <- newTChanIO

  nextId <- newTVarIO 0

  response <- arimaaPost gsurl [("action", "gamestate"), ("sid", sid), ("wait", "0")]

  started <- getUpdates response False updateChan

  case lookup "turn" response of
    Just [c] | Just player <- charToColour c
             , Just s <- lookup (colourToServerChar player : "used") response
             , Just t <- readMaybe s
               -> atomically $ writeTChan updateChan $ UpdateUsed (player, t)
    _ -> return ()

  forM_ (lookup "tcgamenow" response >>= readMaybe) $ atomically . writeTChan updateChan . UpdateGameUsed

  -- TODO: thread handling; error checking
  t1 <- forkIO $ fromServer (gsurl, sid) response started updateChan
  t2 <- forkIO $ toServer (gsurl, sid) (fromJust (lookup "auth" response)) requestChan responseChan

  postGUIAsync
    $ newGame GameParams{names = mapColourArray $ \c -> if c == Protocol.role gameInfo then "me" else Protocol.opponent gameInfo
                        ,ratings = colourArray $ map (\s -> lookup s response >>= readMaybe) ["wrating", "brating"]
                        ,isUser = mapColourArray (== Protocol.role gameInfo)
                        ,timeControl = Protocol.timecontrol gameInfo
                        ,rated = Protocol.rated gameInfo
                        }
              []
              (sendMessage requestChan nextId)
              (Just <$> channelEvent responseChan)
              (do
                  e <- channelEvent updateChan
                  b <- accumB newGameState (flip updateGameState <$> e)
                  return (b, e))
              (mapM_ killThread [t1, t2])

watchGame :: String -> IO ()
watchGame gid = handle (\(Protocol.ServerError s) -> alert s) $ do
  gameroom <- gameroom
  ri <- Protocol.reserveView gameroom gid
  ((gsurl, sid), _, _) <- sit gameroom ri

  updateChan <- newTChanIO

  response <- arimaaPost gsurl [("action", "gamestate"), ("sid", sid), ("wait", "0")]

  started <- getUpdates response False updateChan

  case lookup "turn" response of
    Just [c] | Just player <- charToColour c
             , Just s <- lookup (colourToServerChar player : "used") response
             , Just t <- readMaybe s
               -> atomically $ writeTChan updateChan $ UpdateUsed (player, t)
    _ -> return ()

  forM_ (lookup "tcgamenow" response >>= readMaybe) $ atomically . writeTChan updateChan . UpdateGameUsed

  -- TODO: thread handling; error checking
  t1 <- forkIO $ fromServer (gsurl, sid) response started updateChan

  let tc = fromMaybe (fromJust (parseTimeControl "0/0")) $ do
        s <- lookup "timecontrol" response
        [_, s'] <- matchRegex (mkRegex "^(. )?(.*)") s
        parseTimeControl s'

  postGUIAsync
                   -- TODO: handle missing values
                   -- TODO: strip * from player names
    $ newGame GameParams{names = colourArray $ map (\s -> fromJust (lookup s response)) ["wplayer", "bplayer"]
                        ,ratings = colourArray $ map (\s -> lookup s response >>= readMaybe) ["wrating", "brating"]
                        ,isUser = mapColourArray $ const False
                        ,timeControl = tc
                        ,rated = fromJust (lookup "rated" response) == "1"
                        }
              [] (\_ -> return ()) (return Nothing)
              (do
                  e <- channelEvent updateChan
                  b <- accumB newGameState (flip updateGameState <$> e)
                  return (b, e))
              (killThread t1)

----------------------------------------------------------------

updateServerGames :: IO ()
updateServerGames = forever $ do
  gameroom <- gameroom
  Protocol.myGames gameroom >>= atomically . writeTVar (get myGames)
  Protocol.openGames gameroom >>= atomically . writeTVar (get openGames)
  getLiveGames "http://arimaa.com/arimaa/gameroom/watchgames.cgi" >>= atomically . writeTVar (get liveGames)
  getLiveGames "http://arimaa.com/arimaa/gameroom/postalgames.cgi" >>= atomically . writeTVar (get postalGames)
  threadDelay (30 * 10^6)

makeTreeStore :: Forest a -> [(String, a -> [AttrOp CellRendererText])] -> IO (TreeStore a, TreeView)
makeTreeStore forest l = do
  ts <- treeStoreNew forest
  tv <- treeViewNewWithModel ts
  cr <- cellRendererTextNew
  forM_ l $ \(s, g) -> do
    col <- treeViewColumnNew
    treeViewAppendColumn tv col
    treeViewColumnSetTitle col s
    cellLayoutPackStart col cr False
    cellLayoutSetAttributes col cr ts g
  return (ts, tv)

serverGameCallback :: [Protocol.GameInfo] -> IO ()
serverGameCallback games = do
  d <- dialogNew
  Gtk.set d [windowTransientFor := get window
            ,windowDefaultWidth := 400
            ,windowDefaultHeight := 400
            ]

  dialogAddButton d "Cancel" ResponseCancel
  dialogAddButton d "Open game" ResponseOk
  u <- castToContainer <$> dialogGetContentArea d

  (ts, tv) <- makeTreeStore (map (\g -> Node g []) games)
                            [("Opponent", \gi -> [cellText := Protocol.opponent gi])
                            ,("Role", \gi -> [cellText := show (Protocol.role gi)])
                            ,("Time control", \gi -> [cellText := show (Protocol.timecontrol gi)])
                            ,("Rated", \gi -> [cellText := if Protocol.rated gi then "R" else "U"])
                            ]

  treeViewExpandAll tv

  sw <- scrolledWindowNew Nothing Nothing
  Gtk.set sw [widgetVExpand := True
             ,widgetHeightRequest := 100
             ]
  containerAdd sw tv
  containerAdd u sw

  widgetShowAll d
  
  d `on` response $ \case
    ResponseOk -> treeViewGetSelection tv >>= treeSelectionGetSelected >>= \case
      Nothing -> return ()
      Just iter -> treeModelGetPath ts iter >>= treeStoreGetValue ts >>= \game -> do
        forkIO $ withStatus "Starting game" $ setServerGame game
        widgetDestroy d
    _ -> widgetDestroy d

  return ()

watchGameCallback :: IO ()
watchGameCallback = do
  liveGames <- readTVarIO (get liveGames)
  postalGames <- readTVarIO (get postalGames)
  d <- dialogNew
  Gtk.set d [windowTransientFor := get window
            ,windowDefaultWidth := 600
            ,windowDefaultHeight := 500
            ]

  dialogAddButton d "Cancel" ResponseCancel
  dialogAddButton d "Watch game" ResponseOk
  u <- castToContainer <$> dialogGetContentArea d

  (ts, tv) <- makeTreeStore [Node (Left "Live games") (map (\g -> Node (Right g) []) liveGames)
                            ,Node (Left "Postal games") (map (\g -> Node (Right g) []) postalGames)
                            ]
                            [("Gold", \x -> [cellText := either id (\lgi -> printf "%s (%d)"
                                                                                   (liveNames lgi ! Gold)
                                                                                   (liveRatings lgi ! Gold))
                                                                x])
                            ,("Silver", \x -> [cellText := either (const "") (\lgi -> printf "%s (%d)"
                                                                                             (liveNames lgi ! Silver)
                                                                                             (liveRatings lgi ! Silver))
                                                                  x])
                            ,("Time control", \x -> [cellText := either (const "") (show . liveTimeControl) x])
                            ,("Rated", \x -> [cellText := either (const "") (\lgi -> if liveRated lgi then "R" else "U") x])
                            ]

  treeViewExpandAll tv

  sw <- scrolledWindowNew Nothing Nothing
  Gtk.set sw [widgetVExpand := True
             ,widgetHeightRequest := 100
             ]
  containerAdd sw tv
  containerAdd u sw

  widgetShowAll d

  d `on` response $ \case
    ResponseOk -> treeViewGetSelection tv >>= treeSelectionGetSelected >>= \case
      Nothing -> return ()
      Just iter -> treeModelGetPath ts iter >>= treeStoreGetValue ts >>= \case
        Left _ -> return ()
        Right lgi -> do
          forkIO $ withStatus "Starting game" $ watchGame $ liveGid lgi
          widgetDestroy d
    _ -> widgetDestroy d

  return ()

----------------------------------------------------------------

makeDialog :: WidgetClass w => w -> String -> IO Bool -> IO ()
makeDialog w buttonText f = do
  d <- dialogNew
  Gtk.set d [windowTransientFor := get window]
  dialogAddButton d "Cancel" ResponseCancel
  widgetGrabDefault =<< dialogAddButton d buttonText ResponseOk
  u <- castToContainer <$> dialogGetContentArea d
  containerAdd u w
  widgetShowAll d
  d `on` response $ \case
    ResponseOk -> f >>= \case
      True -> widgetDestroy d
      False -> return ()
    _ -> widgetDestroy d
  return ()

viewGameCallback :: IO ()
viewGameCallback = do
  (w,g,_) <- entryAccessor "Enter game id:"
  makeDialog w "View game" $ g >>= \s -> case readMaybe s of
    Just n -> do
      viewGame n
      return True
    _ -> return False

data ServerGameInfo = ServerGameInfo {sgiNames :: Array Colour String
                                     ,sgiRatings :: Array Colour Int
                                     ,sgiTimeControl :: TimeControl
                                     ,sgiRated :: Bool
                                     ,sgiResult :: (Colour, Reason)
                                     ,sgiMoves :: [(GenMove, Int)]
                                     }

maybeFail x = do
  Just a <- x
  return a


-- parseMovestring :: String -> Maybe [GenMove]
-- parseMovestring = traverse parseGenMove . splitOn "\\n"

-- movesParser :: P.Parsec String () [GenMove]
-- movesParser = P.sepBy (some P.digit *> P.oneOf "wbgs" *> some P.space *> (P.try (Left <$> setup) <|> Right <$> move))
--                       (P.string "\\n")
--   where
--     piece = maybeFail $ charToPiece <$> P.letter
--     square = maybeFail $ (\c1 c2 -> stringToSquare [c1,c2]) <$> P.letter <*> P.digit
--     dir = P.choice [Just North <$ P.char 'n'
--                  ,Just East <$ P.char 'e'
--                  ,Just South <$ P.char 's'
--                  ,Just West <$ P.char 'w'
--                  ,Nothing <$ P.char 'x'
--                  ]
--     setup = P.sepBy1 (flip (,) <$> piece <*> square) (some P.space)
--     move = Move <$> P.sepBy1 ((,,) <$> piece <*> square <*> dir) (some P.space)

getServerGame :: Int -> IO (Maybe ServerGameInfo)
getServerGame n = do
  s <- getResponseBody =<< (simpleHTTP $ getRequest $ "http://arimaa.com/arimaa/gameroom/opengamewin.cgi?gameid=" ++ show n)
  let f s = case matchRegex (mkRegex "arimaa\\.vars\\.(.*)=\"(.*)\"") s of
        Just [a, b] -> Just (a, b)
        _ -> Nothing
      assocs = mapMaybe f $ lines s
  return $ do
    [wplayer, bplayer, wrating, brating, timecontrol, rated, result, reason, movelist, timeused]
      <- mapM (flip lookup assocs)
              ["wplayer", "bplayer", "wrating", "brating", "timecontrol", "rated", "result", "reason", "movelist", "timeused"]
    rs <- mapM readMaybe [wrating, brating]
    tc <- parseTimeControl timecontrol
    r <- case reason of [c] -> readReason c; _ -> Nothing
    let ms = mapMaybe readGenMove $ splitOn "\\n" movelist
--    ms <- either (const Nothing) Just $ P.parse movesParser "" movelist
    ts <- mapM readMaybe $ splitOn " " timeused
    Just ServerGameInfo{sgiNames = colourArray [wplayer, bplayer]
                       ,sgiRatings = colourArray rs
                       ,sgiTimeControl = tc
                       ,sgiRated = rated == "1"
                       ,sgiResult = (if elem result ["w","g"] then Gold else Silver, r)
                       ,sgiMoves = zip ms ts
                       }

mapAccumLM :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapAccumLM f x l = foldM (\(a,cs) g -> fmap (\(a',c) -> (a',cs++[c])) (g a)) (x,[]) (map (flip f) l)

expandGame :: TimeControl -> [(GenMove, Maybe Int)] -> Either String [GameTreeNode]
expandGame tc moves = snd <$> mapAccumLM f (newPosition, Just (initialReserve tc)) moves
  where
    f :: (Position, Maybe Int) -> (GenMove, Maybe Int) -> Either String ((Position, Maybe Int), GameTreeNode)
    f (p, r) (m, t) = fmap (\p' -> let r' = updateReserve tc <$> t <*> r in ((p', r'), GameTreeNode m p' t r'))
                             (playGenMove p m)

viewGame :: Int -> IO ()
viewGame n = do
  background (withStatus "Fetching game" (getServerGame n)) $ \(Just sgi) -> do  -- TODO: check return value
    let nodes = either error id $ expandGame (sgiTimeControl sgi) (map (second Just) (sgiMoves sgi))
        pos = if null nodes then newPosition else nodePosition (last nodes)

    newGame GameParams{names = sgiNames sgi
                      ,ratings = Just <$> sgiRatings sgi
                      ,isUser = mapColourArray (const False)
                      ,timeControl = sgiTimeControl sgi
                      ,rated = sgiRated sgi
                      }
            (foldr (\n f -> [Node n f]) [] nodes)
            (\_ -> return ()) (return Nothing)
            (return (pure GameState{started = True, position = pos, result = Just $ sgiResult sgi},
                     never))
            (return ())

----------------------------------------------------------------

data BotLadderBot = BotLadderBot
  {botName :: String
  ,botUrl :: String
  ,botRating, botRatingk :: Int
  ,nGames, nGold, nSilver, nWon, nLost :: Int
  } deriving Show

botLadderAll :: Maybe String -> IO [BotLadderBot]
botLadderAll name = do
  s <- getResponseBody =<< simpleHTTP (getRequest ("http://arimaa.com/arimaa/gameroom/botLadderAll.cgi" ++ maybe "" ("?u=" ++) name)) 
  let trs = tail [l | TagBranch "tr" _ l <- universeTree $ parseTree s]
      f tr = BotLadderBot{..}
        where
          tds = [m | TagBranch "td" _ m <- tr]
          [botName] = [s | TagBranch "a" _ [TagLeaf (TagText s)] <- tds !! 0]
          [botUrl] = [s | TagBranch "a" [("href", s)] _ <- tds !! 3]
          [botRating, botRatingk, nGames, nGold, nSilver, nWon, nLost]
            = map (read . (\case [TagLeaf (TagText x)] -> x; _ -> "0") . (tds !!)) [1,2,5,6,7,8,9]
  return $ map f trs

getBotLadder :: IO ()
getBotLadder = do
  u <- fromMaybe "" <$> getSetting username
  a <- async (botLadderAll (if null u then Nothing else Just u))
  atomically $ writeTVar (get botLadderBotsRef) $ wait a

startBot :: String -> Colour -> IO ()
startBot url c = do
  s <- getResponseBody =<< simpleHTTP (postRequestWithBody url "application/x-www-form-urlencoded"
                                                           (urlEncodeVars [("action", "player")
                                                                          ,("newgame", "Start Bot")
                                                                          ,("side", colourToServerChar c : [])
                                                                          ]))
  when (s /= "Bot started.\n") $ alert s

----------------------------------------------------------------

playBot :: BotLadderBot -> Colour -> IO ()
playBot bot c = void $ forkIO $ do
  withStatus "Starting bot" $ startBot (botUrl bot) (flipColour c)
  gameroom <- gameroom
  openGames <- withStatus "Contacting gameroom" $ Protocol.openGames gameroom
  case find (\gi -> Protocol.opponent gi == botName bot && Protocol.role gi == c) openGames of
    Nothing -> error "Missing game"   -- TODO: something better
    Just game -> withStatus "Starting game" $ setServerGame game

playBotCallback :: IO ()
playBotCallback = do
  d <- dialogNew
  Gtk.set d [windowTransientFor := get window
            ,windowDefaultWidth := 300
            ,windowDefaultHeight := 500
            ]
  dialogAddButton d "Cancel" ResponseCancel
  dialogAddButton d "Play bot" ResponseOk
  u <- castToContainer <$> dialogGetContentArea d

  l <- labelNew (Just "Play as:")
  Gtk.set l [miscXalign := 0]
  containerAdd u l

  goldButton <- radioButtonNewWithLabel "Gold"
  silverButton <- radioButtonNewWithLabelFromWidget goldButton "Silver"
  mapM_ (containerAdd u) [goldButton, silverButton]

  bots <- botLadderBots
  let speeds = ["Lightning", "Blitz", "Fast", "P2", "P1", "CC"]
      (rest, bits) = mapAccumL (\bs s -> partition (not . (s `isSuffixOf`) . botName) bs) bots speeds
  (ts, tv) <- makeTreeStore (zipWith (\speed bit -> Node (Left speed) (map (\bot -> Node (Right bot) []) bit))
                                    (speeds ++ ["Rest"])
                                    (bits ++ [rest]))
                            [("Bot", \x -> [cellText := either id botName x])
                            ,("Rating", \x -> [cellText := either (const "") (show . botRating) x])
                            ]

  sw <- scrolledWindowNew Nothing Nothing
  Gtk.set sw [widgetVExpand := True
             ,widgetHeightRequest := 100
             ]
  containerAdd sw tv
  containerAdd u sw

  widgetShowAll d
  
  d `on` response $ \case
    ResponseOk -> treeViewGetSelection tv >>= treeSelectionGetSelected >>= \case
      Nothing -> return ()
      Just iter -> treeModelGetPath ts iter >>= treeStoreGetValue ts >>= \case
        Left _ -> return ()
        Right bot -> fmap fst . find snd . zip [Gold, Silver] <$> mapM toggleButtonGetActive [goldButton, silverButton] >>= \case
          Nothing -> return ()
          Just c -> do
            widgetDestroy d
            playBot bot c
    _ -> widgetDestroy d

  return ()

----------------------------------------------------------------

rmTag :: String -> [TagTree String] -> [TagTree String]
rmTag s [] = []
rmTag s (TagBranch s' as l : ts) | s == s' = rmTag s ts
                                 | otherwise = TagBranch s' as (rmTag s l) : rmTag s ts
rmTag s (tl : ts) = tl : rmTag s ts

data LiveGameInfo = LiveGameInfo
  {liveNames :: Array Colour String
  ,liveRatings :: Array Colour Int
  ,liveTimeControl :: TimeControl
  ,liveRated :: Bool
  ,liveGid :: String
  } deriving Show

getLiveGames :: String -> IO [LiveGameInfo]
getLiveGames url = do
  s <- getResponseBody =<< simpleHTTP (getRequest url)
  let table = head [l | TagBranch "table" _ l <- universeTree $ parseTree s]
      g tr | any (\case {TagBranch "table" _ _ -> True; _ -> False}) (universeTree tr) = Right tr
           | otherwise = Left $ head [s | TagLeaf (TagText s) <- universeTree tr, not (all isSpace s)]
      trs = [r | Right r <- takeWhile (/= Left "Scheduled Games") $ map g $ drop 2 [l | TagBranch "tr" _ l <- table]]
      f tr = LiveGameInfo{liveNames = colourArray [gn, sn]
                         ,liveRatings = colourArray $ map parseRating [gr, sr]
                         ,liveTimeControl
                         ,liveRated
                         ,liveGid
                         }
        where
             -- <sup> tags contain move numbers on the postal games page
          a = [s | TagLeaf (TagText s) <- universeTree (rmTag "sup" tr), not (all isSpace s)]
          (r, [gn, gr, tc, sn, sr]) = partition (== "R") a
          liveRated = not (null r)
          Just liveTimeControl = parseTimeControl tc
          parseRating = head . mapMaybe readMaybe . words
          ([liveGid]:_) = mapMaybe (matchRegex (mkRegex "openGame.*\\('([[:digit:]]+)"))
                       [fromJust (lookup "href" attrs) | TagBranch "a" attrs _ <- universeTree tr]
  return $ map f trs

----------------------------------------------------------------

requestToUpdate RequestStart = UpdateStart
requestToUpdate (RequestMove m) = UpdateMove (m, Nothing)
requestToUpdate (RequestResign c) = UpdateResult (flipColour c, Resignation)

dummyGame :: TimeControl -> IO ()
dummyGame tc = do
  (ah, fire) <- newAddHandler
  newGame GameParams{names = mapColourArray (const "me")
                    ,ratings = mapColourArray (const Nothing)
                    ,isUser = mapColourArray (const True)
                    ,timeControl = tc
                    ,rated = False
                    }
          [] fire (return Nothing)
          (do
             e <- fromAddHandler ah
             let u = requestToUpdate <$> e
             b <- accumB newGameState (flip updateGameState <$> u)
             return (b, u)
          )
          (return ())

----------------------------------------------------------------

promptUsername :: IO () -> IO ()
promptUsername finalAction = do
  d <- dialogNew
  Gtk.set d [windowTransientFor := get window]
  widgetGrabDefault =<< dialogAddButton d "OK" ResponseOk

  u <- castToContainer <$> dialogGetContentArea d

  l <- labelNew (Just "Enter gameroom stuff:")
  containerAdd u l
  [e1, e2] <- forM ["Username:", "Password:"] $ \s -> do
    b <- hBoxNew False 5
    l <- labelNew (Just s)
    e <- entryNew
    entrySetActivatesDefault e True
    containerAdd b l
    containerAdd b e
    containerAdd u b
    return e
    
  widgetShowAll d

  d `on` response $ \_ -> do
    u <- entryGetText e1
    p <- entryGetText e2
    widgetDestroy d
    setUsernameAndPassword u p
    finalAction

  return ()

initialStuff :: IO ()
initialStuff = do
  let x = do
        getBotLadder
        void $ forkIO updateServerGames
  u <- getSetting username
  p <- getSetting password
  case (u, p) of
    (Just _, Just _) -> x
    _ -> promptUsername x

----------------------------------------------------------------

settingAccessor :: (Show a, Read a)
                => Settings.Setting a
                -> IO a
                -> (a -> IO ())
                -> (IO (Settings.Conf -> Settings.Conf), Settings.Conf -> IO ())
settingAccessor s getS setS = ((\x c -> Settings.setSetting c s x) <$> getS,
                               \c -> setS (Settings.getSetting' c s)
                              )

widgetsToConf :: IO (Settings.Conf -> Settings.Conf)
confToWidgets :: Settings.Conf -> IO ()
(widgetsToConf, confToWidgets) = (foldr (liftA2 (.)) (return id) gets,
                                  foldr (\x y c -> x c >> y c) (const (return ())) sets)
  where
    (gets, sets) = unzip [settingAccessor username (Just <$> entryGetText (get usernameEntry))
                                                   (entrySetText (get usernameEntry) . fromMaybe "")
                         ,settingAccessor password (Just <$> entryGetText (get passwordEntry))
                                                   (entrySetText (get passwordEntry) . fromMaybe "")
                         ,settingAccessor viewMySide (fromMaybe False . fmap fst . find snd . zip [True, False]
                                                           <$> mapM toggleButtonGetActive [get mySideButton, get goldSideButton])
                                                     (\v -> toggleButtonSetActive (if v then get mySideButton else get goldSideButton)
                                                                                  True)
                         ,settingAccessor enablePlans (toggleButtonGetActive (get enablePlansButton))
                                                      (toggleButtonSetActive (get enablePlansButton))
                         ,settingAccessor killPlans (toggleButtonGetActive (get killPlansButton))
                                                    (toggleButtonSetActive (get killPlansButton))
                         ]

settingsButtonCallback :: ListStore (String, ([Modifier], KeyVal)) -> IO ()
settingsButtonCallback ls = do
  readTVarIO (get conf) >>= confToWidgets

    -- use of dialogRun to prevent the dialog from being destroyed on deleteEvent
  dialogRun (get settingsDialog) >>= \_ -> settingsSetCallback ls

initKeyList :: IO (ListStore (String, ([Modifier], KeyVal)))
initKeyList = do
  ls <- listStoreNew =<< mapM (\(setting, desc, _) -> (desc,) <$> getSetting setting) keyBindings
  treeViewSetModel (get keyTreeView) ls

  crt <- cellRendererTextNew
  cellLayoutPackStart (get actionColumn) crt False
  cellLayoutSetAttributes (get actionColumn) crt ls $ \(a,_) -> [cellText := a]

  cra <- cellRendererAccelNew
  Gtk.set cra [cellRendererAccelAccelMode := CellRendererAccelModeOther]

  cellLayoutPackStart (get accelColumn) cra False
  cellLayoutSetAttributes (get accelColumn) cra ls $ \(_,(m,k)) -> [cellRendererAccelAccelKey := fromIntegral k
                                                                   ,cellRendererAccelAccelMods := m
                                                                   ]

  get keyTreeView `on` keyPressEvent $ do
    k <- eventKeyVal
    m <- eventModifier
    liftIO $ do
      treeViewGetSelection (get keyTreeView) >>= treeSelectionGetSelected >>= \case
        Nothing -> return ()
        Just iter -> do
          (a,b) <- listStoreGetValue ls (listStoreIterToIndex iter)
          listStoreSetValue ls (listStoreIterToIndex iter) (a, (m,k))
    return True

  return ls

settingsSetCallback :: ListStore (String, ([Modifier], KeyVal)) -> IO ()
settingsSetCallback ls = do
  c <- readTVarIO (get conf)
  c' <- ($ c) <$> widgetsToConf

  l <- listStoreToList ls
  let c'' = foldl' (\c ((s,_,_),(_,mk)) -> Settings.setSetting c s mk)
                   c'
                   $ zip keyBindings l

  let f c = (Settings.getSetting' c username, Settings.getSetting' c password)
      (u, p) = f c''
  when (f c /= (u, p)) $ setUsernameAndPassword (fromMaybe "" u) (fromMaybe "" p)
  
  get setConf c''
  
  widgetHide (get settingsDialog)

----------------------------------------------------------------

#ifdef LOCAL
dataFileName = return
#else
dataFileName = getDataFileName
#endif

main = do
  initGUI

  icons <- fmap (listArray ((Gold, 0), (Silver, length pieceInfo - 1)))
                $ mapM ((>>= imageSurfaceCreateFromPNG) . dataFileName . ("images/" ++))
                       ["GoldRabbit.png"
                       ,"GoldCat.png"
                       ,"GoldDog.png"
                       ,"GoldHorse.png"
                       ,"GoldCamel.png"
                       ,"GoldElephant.png"
                       ,"SilverRabbit.png"
                       ,"SilverCat.png"
                       ,"SilverDog.png"
                       ,"SilverHorse.png"
                       ,"SilverCamel.png"
                       ,"SilverElephant.png"
                       ]

  builder <- builderNew
  builderAddFromFile builder =<< dataFileName "client.glade"

  window <- builderGetObject builder castToWindow "window"{-# LINE 2042 "Main.vhs" #-}
  sendButton <- builderGetObject builder castToButton "send-button"{-# LINE 2043 "Main.vhs" #-}
  planButton <- builderGetObject builder castToButton "plan-button"{-# LINE 2043 "Main.vhs" #-}
  resignButton <- builderGetObject builder castToButton "resign-button"{-# LINE 2043 "Main.vhs" #-}
  boardCanvas <- builderGetObject builder castToDrawingArea "board-canvas"{-# LINE 2044 "Main.vhs" #-}
  captureCanvas <- builderGetObject builder castToDrawingArea "capture-canvas"{-# LINE 2044 "Main.vhs" #-}
  treeCanvas <- builderGetObject builder castToDrawingArea "tree-canvas"{-# LINE 2044 "Main.vhs" #-}
  gameLabel <- builderGetObject builder castToLabel "game-label"{-# LINE 2045 "Main.vhs" #-}
  topPlayer <- builderGetObject builder castToLabel "top-player"{-# LINE 2045 "Main.vhs" #-}
  bottomPlayer <- builderGetObject builder castToLabel "bottom-player"{-# LINE 2045 "Main.vhs" #-}
  gameClock <- builderGetObject builder castToLabel "game-clock"{-# LINE 2045 "Main.vhs" #-}
  topClock <- builderGetObject builder castToLabel "top-clock"{-# LINE 2045 "Main.vhs" #-}
  bottomClock <- builderGetObject builder castToLabel "bottom-clock"{-# LINE 2045 "Main.vhs" #-}
  statusLabel <- builderGetObject builder castToLabel "status-label"{-# LINE 2046 "Main.vhs" #-}
  harlogLabel <- builderGetObject builder castToLabel "harlog-label"{-# LINE 2046 "Main.vhs" #-}
  moveLabel <- builderGetObject builder castToLabel "move-label"{-# LINE 2046 "Main.vhs" #-}
  topUsedClock <- builderGetObject builder castToLabel "top-used-clock"{-# LINE 2046 "Main.vhs" #-}
  bottomUsedClock <- builderGetObject builder castToLabel "bottom-used-clock"{-# LINE 2046 "Main.vhs" #-}
  setupGrid <- builderGetObject builder castToGrid "setup-grid"{-# LINE 2047 "Main.vhs" #-}
  captureGrid <- builderGetObject builder castToGrid "capture-grid"{-# LINE 2047 "Main.vhs" #-}
  myGamesItem <- builderGetObject builder castToMenuItem "my-games-item"{-# LINE 2048 "Main.vhs" #-}
  openGamesItem <- builderGetObject builder castToMenuItem "open-games-item"{-# LINE 2048 "Main.vhs" #-}
  watchGamesItem <- builderGetObject builder castToMenuItem "watch-games-item"{-# LINE 2048 "Main.vhs" #-}
  viewGameItem <- builderGetObject builder castToMenuItem "view-game-item"{-# LINE 2048 "Main.vhs" #-}
  playBotItem <- builderGetObject builder castToMenuItem "play-bot-item"{-# LINE 2048 "Main.vhs" #-}
  flipBoard <- builderGetObject builder castToMenuItem "flip-board"{-# LINE 2048 "Main.vhs" #-}
  blindModeMenu <- builderGetObject builder castToMenu "blind-mode-menu"{-# LINE 2049 "Main.vhs" #-}
  settingsItem <- builderGetObject builder castToMenuItem "settings-item"{-# LINE 2050 "Main.vhs" #-}
  settingsDialog <- builderGetObject builder castToDialog "settings-dialog"{-# LINE 2051 "Main.vhs" #-}
  treeGrid <- builderGetObject builder castToGrid "tree-grid"{-# LINE 2052 "Main.vhs" #-}
  mainGrid <- builderGetObject builder castToGrid "main-grid"{-# LINE 2052 "Main.vhs" #-}
  usernameEntry <- builderGetObject builder castToEntry "username-entry"{-# LINE 2053 "Main.vhs" #-}
  passwordEntry <- builderGetObject builder castToEntry "password-entry"{-# LINE 2053 "Main.vhs" #-}
  goldSideButton <- builderGetObject builder castToRadioButton "gold-side-button"{-# LINE 2054 "Main.vhs" #-}
  mySideButton <- builderGetObject builder castToRadioButton "my-side-button"{-# LINE 2054 "Main.vhs" #-}
  startButton <- builderGetObject builder castToButton "start-button"{-# LINE 2055 "Main.vhs" #-}
  prevButton <- builderGetObject builder castToButton "prev-button"{-# LINE 2055 "Main.vhs" #-}
  currentButton <- builderGetObject builder castToButton "current-button"{-# LINE 2055 "Main.vhs" #-}
  nextButton <- builderGetObject builder castToButton "next-button"{-# LINE 2055 "Main.vhs" #-}
  endButton <- builderGetObject builder castToButton "end-button"{-# LINE 2055 "Main.vhs" #-}
  deleteNodeButton <- builderGetObject builder castToButton "delete-node-button"{-# LINE 2055 "Main.vhs" #-}
  deleteAllButton <- builderGetObject builder castToButton "delete-all-button"{-# LINE 2055 "Main.vhs" #-}
  treeScrolledWindow <- builderGetObject builder castToScrolledWindow "tree-scrolled-window"{-# LINE 2056 "Main.vhs" #-}
  actionColumn <- builderGetObject builder castToTreeViewColumn "action-column"{-# LINE 2057 "Main.vhs" #-}
  accelColumn <- builderGetObject builder castToTreeViewColumn "accel-column"{-# LINE 2057 "Main.vhs" #-}
  keyTreeView <- builderGetObject builder castToTreeView "key-tree-view"{-# LINE 2058 "Main.vhs" #-}
  enablePlansButton <- builderGetObject builder castToCheckButton "enable-plans-button"{-# LINE 2059 "Main.vhs" #-}
  killPlansButton <- builderGetObject builder castToCheckButton "kill-plans-button"{-# LINE 2059 "Main.vhs" #-}

  setupIcons <- replicateM (length pieceInfo) drawingAreaNew
  setupLabels <- replicateM (length pieceInfo) $ labelNew (Nothing :: Maybe String)

  let x = div 140 6  -- TODO: get size automatically
    in forM_ setupIcons $ flip Gtk.set [widgetWidthRequest := x, widgetHeightRequest := x]

  zipWithM_ (\i n -> gridAttach setupGrid i n 0 1 1) setupIcons [0..]
  zipWithM_ (\l n -> gridAttach setupGrid l n 1 1 1) setupLabels [0..]

  (getBlindMode, blindModeAH) <- do
    first <- radioMenuItemNewWithLabel "Sighted"
    rest <- mapM (radioMenuItemNewWithLabelFromWidget first) ["Blind", "Show friendly", "Show enemy"]
    let l = [(True, True), (False, False), (True, False), (False, True)]
        f = do
          x <- mapM checkMenuItemGetActive (first : rest)
          return $ fromMaybe (True, True) $ snd <$> find fst (zip x l)
    (ah, fire) <- newAddHandler
    zipWithM_ (\item state -> do
                  containerAdd blindModeMenu item
                  item `on` checkMenuItemToggled $ do
                    b <- checkMenuItemGetActive item
                    when b $ fire state
              )
              (first : rest) l
    return (f, ah)
  
  widgetAddEvents boardCanvas [ButtonPressMask, ButtonReleaseMask, Button1MotionMask]
  widgetAddEvents window [KeyPressMask]
  widgetAddEvents treeCanvas [ButtonPressMask]

  let layoutHack = True

  when layoutHack $ do
    sizeRef <- newIORef Nothing

    window `on` configureEvent $ do
      s <- eventSize
      liftIO $ readIORef sizeRef >>= \case
        Just s' | s' == s -> return ()
        _ -> do
          Gtk.set boardCanvas [widgetWidthRequest := 150
                              ,widgetHeightRequest := 150
                              ,widgetExpand := True
                              ]
          Gtk.set topClock [widgetHExpand := False]
          writeIORef sizeRef (Just s)

      return False

    boardCanvas `on` sizeAllocate $ \(Rectangle _ _ x y) -> postGUIAsync $ do
      let z = min x y
      Gtk.set boardCanvas [widgetWidthRequest := z
                          ,widgetHeightRequest := z
                          ,widgetExpand := False
                          ]
      Gtk.set topClock [widgetHExpand := True]
    
    return ()

  let makeDraw :: WidgetClass w => w -> IO ((w -> Render ()) -> IO ())
      makeDraw w = do
        drawRef <- newIORef $ return ()
        w `on` draw $ join (liftIO (readIORef drawRef))
        return $ \r -> do
          writeIORef drawRef (r w)
          widgetQueueDraw w

  setDrawBoard <- makeDraw boardCanvas
  setDrawSetupIcons <- mapM makeDraw setupIcons
  setDrawCapture <- makeDraw captureCanvas
  setDrawTree <- makeDraw treeCanvas

  setDrawBoard $ \canvas -> do
    x <- liftIO $ squareSize canvas
    setSourceRGB 1 1 1
    paint
    translate borderWidth borderWidth
    scale x x

    drawEmptyBoard

  (leftPressAH, leftPressFire) <- newAddHandler
  (rightPressAH, rightPressFire) <- newAddHandler
  (motionAH, motionFire) <- newAddHandler
  (releaseAH, releaseFire) <- newAddHandler
  (flipAH, flipFire) <- newAddHandler

  setupIconAH <- forM setupIcons $ \icon -> do
    (ah, fire) <- newAddHandler
    icon `on` buttonPressEvent $ do {liftIO $ fire (); return True}
    return ah

  (tickAH, tickFire) <- newAddHandler
  timeoutAdd (True <$ tickFire ()) (div 1000 tickFrequency)

----------------------------------------------------------------

  boardCanvas `on` buttonPressEvent $ do
    b <- eventButton
    c <- eventCoordinates
    liftIO $ do
      (sq, x) <- boardCoordinates' boardCanvas c
      if inRange boardRange sq
        then do
          case b of
            LeftButton -> leftPressFire (sq, x)
            RightButton -> rightPressFire sq
          return True
        else return False

  boardCanvas `on` buttonReleaseEvent $ do
    b <- eventButton
    c <- eventCoordinates
    if b == LeftButton
      then liftIO $ do
        boardCoordinates boardCanvas c >>= releaseFire
        return True
      else return False

  boardCanvas `on` motionNotifyEvent $ do
    c <- eventCoordinates
    liftIO $ do
      sq <- boardCoordinates boardCanvas c
      if inRange boardRange sq
        then do
          motionFire sq
          return True
        else return False

  (treePressAH, treePressFire) <- newAddHandler

  treeCanvas `on` buttonPressEvent $ do
    c <- eventCoordinates
    liftIO $ treePressFire c
    return True

  flipBoard `on` menuItemActivated $ flipFire ()

  window `on` deleteEvent $ do
    liftIO $ do
      mainQuit
      readTVarIO (get gameroomRef) >>= \case
        Just g -> Protocol.logout g   -- TODO: timeout or something
        _ -> return ()
    return False

  myGamesItem `on` menuItemActivated $ readTVarIO (get myGames) >>= serverGameCallback
  openGamesItem `on` menuItemActivated $ readTVarIO (get openGames) >>= serverGameCallback
  watchGamesItem `on` menuItemActivated $ watchGameCallback
  viewGameItem `on` menuItemActivated $ viewGameCallback
  playBotItem `on` menuItemActivated $ playBotCallback

  widgetGrabDefault =<< dialogAddButton settingsDialog "OK" ResponseOk

----------------------------------------------------------------

  killGameRef <- newIORef (return ())
  statusStack <- newTVarIO []

  myGames <- newTVarIO []
  openGames <- newTVarIO []
  liveGames <- newTVarIO []
  postalGames <- newTVarIO []
  botLadderBotsRef <- newTVarIO (return [])
  gameroomRef <- newTVarIO Nothing

  conf <- newTVarIO Map.empty

  (confAH, confFire) <- newAddHandler
  let setConf c = do
        atomically $ writeTVar conf c
        saveSettings
        confFire c
  
  try (Settings.readSettings settingsPlace) >>= \case
    Right (c, _) -> atomically $ writeTVar conf c
    Left (_ :: IOException) -> return ()

  let buttonSet = ButtonSet{..}
  [sendAH, resignAH, planAH, clearArrowsAH, prevAH, nextAH, startAH, endAH, currentAH, prevBranchAH, nextBranchAH
    ,deleteNodeAH, deleteLineAH, deleteAllAH, deleteFromHereAH]
       <- initKeyActions window buttonSet

  writeIORef globalEnv Env{..}

----------------------------------------------------------------

  do
    ls <- initKeyList
    settingsItem `on` menuItemActivated $ settingsButtonCallback ls

  -- this is on realize so that the prompt-username window is centred over the main window
  window `on` realize $ initialStuff

  widgetShowAll window

  -- user plays self (for testing)
  dummyGame (fromJust (parseTimeControl "1d/30d/100/0/10m/0"))

  mainGUI
