{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleInstances #-}

module Settings where

import Data.AppSettings
import Colour
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy
import Graphics.UI.Gtk
import Data.Maybe
import Data.List
import Control.Applicative
import Data.Semigroup
import Data.Bifunctor

import Templates
import Misc
import WidgetValue

settingsPlace = AutoFromAppName "nosteps"

type Accessor = (IO (Conf -> Conf), Conf -> IO ())

instance {-# OVERLAPS #-} Semigroup Accessor where
  (get1, set1) <> (get2, set2) = (liftA2 (.) get1 get2, liftA2 (>>) set1 set2)

instance {-# OVERLAPS #-} Monoid Accessor where
  mempty = (return id, const (return ()))
  mappend = (<>)

type ConfWidget = Grid -> Int -> Int -> IO (Int, Accessor)

foldConfWidgets :: Grid -> Int -> Int -> [ConfWidget] -> IO (Int, Accessor)
foldConfWidgets grid x y cws = second mconcat <$> mapAccumLM (\y cw -> cw grid x y) y cws

accessor :: (Show a, Read a) => Setting a -> IO a -> (a -> IO ()) -> Accessor
accessor s get set
  = ((\x c -> setSetting c s x) <$> get
    ,\c -> set (getSetting' c s))

defaultConfWidget :: forall a w. (Show a, Read a, WidgetValue a w) => Bool -> String -> Setting a -> ConfWidget
defaultConfWidget labelRight l' s grid x y = do
  (w, get, set) <- makeWidget
  l <- labelNew (Just l')
  miscSetAlignment l 0 0.5
  if labelRight
    then do
      box <- hBoxNew False 5
      boxPackStart box w PackNatural 0
      boxPackStart box l PackNatural 0
      gridAttach grid box x y 2 1
    else do
      gridAttach grid l x y 1 1
      gridAttach grid w (x+1) y 1 1
  return (y + 1, (maybe id (\x c -> setSetting c s x) <$> get
                 ,\c -> set (getSetting' c s)))

viewMySideWidget :: Setting Bool -> ConfWidget
viewMySideWidget s grid x y = do
  box <- hBoxNew False 5
  l <- labelNew (Just "View games from:")
  r1 <- radioButtonNewWithLabel "gold side"
  r2 <- radioButtonNewWithLabelFromWidget r1 "my side"
  boxPackStart box l PackNatural 0
  boxPackStart box r1 PackNatural 0
  boxPackStart box r2 PackNatural 0
  gridAttach grid box x y 2 1
  return (y + 1
         ,accessor s (fromMaybe False . fmap fst . find snd . zip [True, False]
                           <$> mapM toggleButtonGetActive [r2, r1])
                     (\v -> toggleButtonSetActive (if v then r2 else r1) True))

-- intConfWidget :: String -> String -> Int -> Int -> Setting Int -> ConfWidget
-- intConfWidget s1 s2 lo hi s grid x y = do
--   ls@[l1,l2] <- mapM (labelNew . Just) [s1, s2]
--   mapM_ (\l -> miscSetAlignment l 0 0.5)
--   gridAttach grid l1 x y 1 1
--   box <- hBoxNew False 5
--   (b, get, set) <- intWidget lo hi
--   boxPackStart box b PackNatural 0
--   boxPackStart box l2 PackNatural 0
--   gridAttach grid box (x+1) y 1 1
--   return (y + 1, accessor s get set)

----------------------------------------------------------------

--generalSettings :: [ConfWidget]
-- declareSettings "generalSettings"
--   [("username", [t|Maybe String|], [|Nothing|], [|defaultConfWidget False "Username"|])
--   ,("password", [t|Maybe String|], [|Nothing|], [|defaultConfWidget False "Password"|])
--   ,("viewMySide", [t|Bool|], [|False|], [|viewMySideWidget|])
--   ,("enablePlans", [t|Bool|], [|True|], [|defaultConfWidget True "Enable plan variations"|])
--   ,("killPlans", [t|Bool|], [|True|], [|defaultConfWidget True "Kill plans at current position when move comes in"|])
--   ,("pieceSet", [t|PieceSet|], [|ThreeD|], [|defaultConfWidget False "Piece set"|])
--   ,("arrowWidth", [t|Double|], [|0.12|], [|defaultConfWidget False "Arrow width"|])
--   ,("flashTimes", [t|Maybe [Int]|], [|Just [5, 15]|], [|defaultConfWidget False "Flash times"|])
--   ]

username :: Setting (Maybe String)
username = (Setting "username") Nothing
password :: Setting (Maybe String)
password = (Setting "password") Nothing
viewMySide :: Setting Bool
viewMySide = (Setting "view-my-side") False
enablePlans :: Setting Bool
enablePlans = (Setting "enable-plans") True
killPlans :: Setting Bool
killPlans = (Setting "kill-plans") True
pieceSet :: Setting PieceSet
pieceSet = (Setting "piece-set") ThreeD
arrowWidth :: Setting Double
arrowWidth = (Setting "arrow-width") 0.12
flashTimes :: Setting (Maybe [Int])
flashTimes = (Setting "flash-times") (Just [5, 15])
generalSettings
  = [((defaultConfWidget False) "Username") username,
     ((defaultConfWidget False) "Password") password,
     viewMySideWidget viewMySide,
     ((defaultConfWidget True) "Enable plan variations") enablePlans,
     ((defaultConfWidget True)
        "Kill plans at current position when move comes in")
       killPlans,
     ((defaultConfWidget False) "Piece set") pieceSet,
     ((defaultConfWidget False) "Arrow width") arrowWidth,
     ((defaultConfWidget False) "Flash times") flashTimes]

--sharpSettings :: [ConfWidget]
-- declareSettings "sharpSettings"
--   [("sharpExe", [t|Maybe String|], [|Nothing|], [|defaultConfWidget False "Sharp path"|])
--   ,("sharpThreads", [t|Int|], [|1|], [|defaultConfWidget False "Sharp threads"|])
--   ,("sharpTimeLimit", [t|Maybe Int|], [|Just 60|], [|defaultConfWidget False "Sharp time limit"|])
--   ,("sharpDepthLimit", [t|Maybe Int|], [|Nothing|], [|defaultConfWidget False "Sharp depth limit"|])
--   ,("maxSharps", [t|Int|], [|5|], [|defaultConfWidget False "Max Sharps"|])
--   ]

sharpExe :: Setting (Maybe String)
sharpExe = (Setting "sharp-exe") Nothing
sharpThreads :: Setting Int
sharpThreads = (Setting "sharp-threads") 1
sharpTimeLimit :: Setting (Maybe Int)
sharpTimeLimit = (Setting "sharp-time-limit") (Just 60)
sharpDepthLimit :: Setting (Maybe Int)
sharpDepthLimit = (Setting "sharp-depth-limit") Nothing
maxSharps :: Setting Int
maxSharps = (Setting "max-sharps") 5
sharpSettings
  = [((defaultConfWidget False) "Sharp path") sharpExe,
     ((defaultConfWidget False) "Sharp threads") sharpThreads,
     ((defaultConfWidget False) "Sharp time limit") sharpTimeLimit,
     ((defaultConfWidget False) "Sharp depth limit") sharpDepthLimit,
     ((defaultConfWidget False) "Max Sharps") maxSharps]

--colourSettings' :: [(State Conf (), ConfWidget)]
-- declareSettings "colourSettings'" $ map (\(a,b,c,s) -> (a,b,c, [|liftA2 (,) setting (defaultConfWidget False s)|]))
--   [("boardColour1", [t|RGB Double|], [|RGB 1 1 1|], "Board 1")
--   ,("boardColour2", [t|Maybe (RGB Double)|], [|Nothing|], "Board 2")
--   ,("trapColour", [t|RGB Double|], [|RGB 0.9 0.8 0.6|], "Trap")
--   ,("trapGradient", [t|Bool|], [|False|], "Trap gradient")
--   ,("gridColour", [t|Maybe (RGB Double)|], [|Just (RGB 0.5 0.5 0.5)|], "Grid")
--   ,("pieceAlpha", [t|Double|], [|1|], "Piece alpha")
--   ,("liveTrapColour", [t|RGB Double|], [|RGB 1 0 0|], "Live trap")
--   ,("goldColour", [t|RGB Double|], [|RGB 0.9 0.7 0|], "Gold")
--   ,("silverColour", [t|RGB Double|], [|RGB 0.6 0.6 0.8|], "Silver")
--   ,("currentColour", [t|RGB Double|], [|RGB 0 0.7 0|], "Current")
--   ,("viewColour", [t|RGB Double|], [|RGB 0.9 0 0|], "View")
--   ,("runningSharpColour", [t|RGB Double|], [|RGB 0.2 0.9 0.2|], "Running Sharp")
--   ,("pausedSharpColour", [t|RGB Double|], [|RGB 0.3 0.5 0.8|], "Paused Sharp")
--   ,("stoppedSharpColour", [t|RGB Double|], [|RGB 0.9 0.2 0.5|], "Stopped Sharp")
--   ,("goldArrowColour", [t|RGB Double|], [|RGB 1 0 0|], "Gold arrow")
--   ,("silverArrowColour", [t|RGB Double|], [|RGB 0 0 1|], "Silver arrow")
--   ,("invisibleArrowColour", [t|RGB Double|], [|RGB 0 0.9 0|], "Invisible arrow")
--   ,("flashColour", [t|RGB Double|], [|RGB 1 0 0|], "Flash")
--   ]

boardColour1 :: Setting (RGB Double)
boardColour1 = (Setting "board-colour1") (((RGB 1) 1) 1)
boardColour2 :: Setting (Maybe (RGB Double))
boardColour2 = (Setting "board-colour2") Nothing
trapColour :: Setting (RGB Double)
trapColour = (Setting "trap-colour") (((RGB 0.9) 0.8) 0.6)
trapGradient :: Setting Bool
trapGradient = (Setting "trap-gradient") False
gridColour :: Setting (Maybe (RGB Double))
gridColour = (Setting "grid-colour") (Just (((RGB 0.5) 0.5) 0.5))
pieceAlpha :: Setting Double
pieceAlpha = (Setting "piece-alpha") 1
liveTrapColour :: Setting (RGB Double)
liveTrapColour = (Setting "live-trap-colour") (((RGB 1) 0) 0)
goldColour :: Setting (RGB Double)
goldColour = (Setting "gold-colour") (((RGB 0.9) 0.7) 0)
silverColour :: Setting (RGB Double)
silverColour = (Setting "silver-colour") (((RGB 0.6) 0.6) 0.8)
currentColour :: Setting (RGB Double)
currentColour = (Setting "current-colour") (((RGB 0) 0.7) 0)
viewColour :: Setting (RGB Double)
viewColour = (Setting "view-colour") (((RGB 0.9) 0) 0)
runningSharpColour :: Setting (RGB Double)
runningSharpColour
  = (Setting "running-sharp-colour") (((RGB 0.2) 0.9) 0.2)
pausedSharpColour :: Setting (RGB Double)
pausedSharpColour
  = (Setting "paused-sharp-colour") (((RGB 0.3) 0.5) 0.8)
stoppedSharpColour :: Setting (RGB Double)
stoppedSharpColour
  = (Setting "stopped-sharp-colour") (((RGB 0.9) 0.2) 0.5)
goldArrowColour :: Setting (RGB Double)
goldArrowColour = (Setting "gold-arrow-colour") (((RGB 1) 0) 0)
silverArrowColour :: Setting (RGB Double)
silverArrowColour = (Setting "silver-arrow-colour") (((RGB 0) 0) 1)
invisibleArrowColour :: Setting (RGB Double)
invisibleArrowColour
  = (Setting "invisible-arrow-colour") (((RGB 0) 0.9) 0)
flashColour :: Setting (RGB Double)
flashColour = (Setting "flash-colour") (((RGB 1) 0) 0)
colourSettings'
  = [(((liftA2 (,)) setting) ((defaultConfWidget False) "Board 1"))
       boardColour1,
     (((liftA2 (,)) setting) ((defaultConfWidget False) "Board 2"))
       boardColour2,
     (((liftA2 (,)) setting) ((defaultConfWidget False) "Trap"))
       trapColour,
     (((liftA2 (,)) setting)
        ((defaultConfWidget False) "Trap gradient"))
       trapGradient,
     (((liftA2 (,)) setting) ((defaultConfWidget False) "Grid"))
       gridColour,
     (((liftA2 (,)) setting) ((defaultConfWidget False) "Piece alpha"))
       pieceAlpha,
     (((liftA2 (,)) setting) ((defaultConfWidget False) "Live trap"))
       liveTrapColour,
     (((liftA2 (,)) setting) ((defaultConfWidget False) "Gold"))
       goldColour,
     (((liftA2 (,)) setting) ((defaultConfWidget False) "Silver"))
       silverColour,
     (((liftA2 (,)) setting) ((defaultConfWidget False) "Current"))
       currentColour,
     (((liftA2 (,)) setting) ((defaultConfWidget False) "View"))
       viewColour,
     (((liftA2 (,)) setting)
        ((defaultConfWidget False) "Running Sharp"))
       runningSharpColour,
     (((liftA2 (,)) setting) ((defaultConfWidget False) "Paused Sharp"))
       pausedSharpColour,
     (((liftA2 (,)) setting)
        ((defaultConfWidget False) "Stopped Sharp"))
       stoppedSharpColour,
     (((liftA2 (,)) setting) ((defaultConfWidget False) "Gold arrow"))
       goldArrowColour,
     (((liftA2 (,)) setting) ((defaultConfWidget False) "Silver arrow"))
       silverArrowColour,
     (((liftA2 (,)) setting)
        ((defaultConfWidget False) "Invisible arrow"))
       invisibleArrowColour,
     (((liftA2 (,)) setting) ((defaultConfWidget False) "Flash"))
       flashColour]

defaultColours :: Conf
defaultColours = flip execState Map.empty $ mapM_ fst colourSettings'

colourSettings :: [ConfWidget]
colourSettings = map snd colourSettings'
