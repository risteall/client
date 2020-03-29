{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Env where

import Graphics.UI.Gtk hiding (get)
import Reactive.Banana.Frameworks
import Data.Array.IArray
import Graphics.Rendering.Cairo
import Data.IORef
import Control.Concurrent.STM
import Data.Unique
import Data.AppSettings
import System.IO.Unsafe
import System.Process

import qualified Protocol
import Protocol (arimaaPost, Gameroom, PlayInfo, getFields, GameInfo, reserveSeat, sit)
import Scrape
import Base
import WidgetGetter

data ButtonSet = ButtonSet
  {sendButton, planButton, resignButton, sharpButton
  ,startButton, prevButton, currentButton, nextButton, endButton
  ,deleteNodeButton, deleteAllButton :: Button
  }

mkWidgetGetter ''ButtonSet "getButtonSet"

data Widgets = Widgets
  {window :: Window
  ,boardCanvas, captureCanvas, treeCanvas :: DrawingArea
  ,gameLabel, topPlayer, bottomPlayer, gameClock, topClock, bottomClock, statusLabel, harlogLabel, moveLabel, topUsedClock, bottomUsedClock :: Label
  ,setupGrid, captureGrid :: Grid
  ,myGamesItem, openGamesItem, watchGamesItem, viewGameItem, playBotItem, flipBoard :: MenuItem
  ,blindModeMenu :: Menu
  ,settingsItem :: MenuItem
  ,settingsDialog :: Dialog
  ,treeGrid, mainGrid :: Grid
  ,usernameEntry, passwordEntry :: Entry
  ,goldSideButton, mySideButton :: RadioButton
  ,treeScrolledWindow :: ScrolledWindow
  ,actionColumn, accelColumn :: TreeViewColumn
  ,keyTreeView :: TreeView
  ,enablePlansButton, killPlansButton :: CheckButton
  }

mkWidgetGetter ''Widgets "getWidgets"

data Env = Env {buttonSet :: ButtonSet
               ,widgets :: Widgets
               ,icons :: Array (Colour, Int) Surface
               ,setupLabels :: [Label]
               ,leftPressAH :: AddHandler (Square, (Double, Double))
               ,rightPressAH :: AddHandler Square
               ,releaseAH :: AddHandler Square
               ,motionAH :: AddHandler Square
               ,flipAH :: AddHandler ()
               ,tickAH :: AddHandler ()
               ,setupIconAH :: [AddHandler ()]
               ,killGameRef :: IORef (IO ())
               ,setDrawBoard :: (DrawingArea -> Render ()) -> IO ()
               ,setDrawSetupIcons :: [(DrawingArea -> Render ()) -> IO ()]
               ,setDrawCapture :: (DrawingArea -> Render ()) -> IO ()
               ,setDrawTree :: (DrawingArea -> Render ()) -> IO ()
               ,blindModeAH :: AddHandler (Bool, Bool)
               ,botLadderBotsRef :: TVar (IO [BotLadderBot])
               ,statusStack :: TVar [(Unique, String)]
               ,myGames :: TVar [Protocol.GameInfo]
               ,openGames :: TVar [Protocol.GameInfo]
               ,liveGames :: TVar [LiveGameInfo]
               ,postalGames :: TVar [LiveGameInfo]
               ,conf :: TVar Conf
               ,gameroomRef :: TVar (Maybe Gameroom)
               ,treePressAH :: AddHandler (Double, Double)
               ,getBlindMode :: IO (Bool, Bool)
               ,sendAH :: AddHandler ()
               ,resignAH :: AddHandler ()
               ,sharpAH :: AddHandler ()
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
               ,setConf :: Conf -> IO ()
               ,confAH :: AddHandler Conf
               ,initialConf :: Conf
               ,toggleSharpAH :: AddHandler ()
               }

globalEnv :: IORef Env
globalEnv = unsafePerformIO $ newIORef undefined

get :: (Env -> a) -> a
get f = unsafePerformIO $ f <$> readIORef globalEnv

getSetting :: Read a => Setting a -> IO a
getSetting s = do
  c <- readTVarIO (get conf)
  return $ getSetting' c s

getConf :: Read a => Setting a -> a
getConf = getSetting' (get initialConf)

----------------------------------------------------------------

settingsPlace = AutoFromAppName "nosteps"

username :: Setting (Maybe String)
username = Setting "username" Nothing

password :: Setting (Maybe String)
password = Setting "password" Nothing

viewMySide = Setting "view-my-side" False
enablePlans = Setting "enable-plans" True
killPlans = Setting "kill-plans" True

currentColour, viewColour, goldColour, silverColour, lightGoldColour, lightSilverColour
  ,runningSharpColour, pausedSharpColour, stoppedSharpColour, trapColour, liveTrapColour
  ,invisibleArrowColour, goldArrowColour, silverArrowColour :: Setting (Double, Double, Double)

currentColour = Setting "current-colour" (0, 0.7, 0)
viewColour = Setting "view-colour" (0.9, 0, 0)
goldColour = Setting "gold-colour" (0.9, 0.7, 0)
silverColour = Setting "silver-colour" (0.6, 0.6, 0.8)

-- should be determined from dark versions
lightGoldColour = Setting "light-gold-colour" (1, 0.9, 0.5)
lightSilverColour = Setting "light-silver-colour" (0.8, 0.8, 1)

runningSharpColour = Setting "running-sharp-colour" (0.2, 0.9, 0.2)
pausedSharpColour = Setting "paused-sharp-colour" (0.3, 0.5, 0.8)
stoppedSharpColour = Setting "stopped-sharp-colour" (0.9, 0.2, 0.5)

trapColour = Setting "trap-colour" (0.9, 0.8, 0.6)
liveTrapColour = Setting "live-trap-colour" (1, 0, 0)

invisibleArrowColour = Setting "invisible-arrow-colour" (0, 0.9, 0)
goldArrowColour = Setting "gold-arrow-colour" (1, 0, 0)
silverArrowColour = Setting "silver-arrow-colour" (0, 0, 1)

----------------------------------------------------------------

sharpThreads = Setting "sharp-threads" (1 :: Int)
sharpTimeLimit = Setting "sharp-time-limit" (Just (180 :: Int))
sharpDepthLimit = Setting "sharp-depth-limit" (Nothing :: Maybe Int)
maxSharps = Setting "max-sharps" (5 :: Int)

sharpExe = Setting "sharp-exe" (Nothing :: Maybe String)

----------------------------------------------------------------

defaultSettings = getDefaultConfig $ do
  setting currentColour
  setting viewColour
  setting goldColour
  setting silverColour
  setting lightGoldColour
  setting lightSilverColour
  setting runningSharpColour
  setting pausedSharpColour
  setting stoppedSharpColour
  setting trapColour
  setting liveTrapColour
  setting invisibleArrowColour
  setting goldArrowColour
  setting silverArrowColour

  setting sharpThreads
  setting sharpTimeLimit
  setting sharpDepthLimit
  setting maxSharps
  setting sharpExe

saveSettings :: IO ()
saveSettings = readTVarIO (get conf) >>= Data.AppSettings.saveSettings defaultSettings settingsPlace
