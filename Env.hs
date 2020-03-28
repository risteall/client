{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Env where

import Graphics.UI.Gtk hiding (get)
import Reactive.Banana.Frameworks
import Data.Array.IArray
import Graphics.Rendering.Cairo
import Data.IORef
import Control.Concurrent.STM
import Data.Unique
import qualified  Data.AppSettings as Settings
import System.IO.Unsafe
import System.Process

import qualified Protocol
import Protocol (arimaaPost, Gameroom, PlayInfo, getFields, GameInfo, reserveSeat, sit)
import Scrape
import Base
import Sharp
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
               ,conf :: TVar Settings.Conf
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
               ,setConf :: Settings.Conf -> IO ()
               ,confAH :: AddHandler Settings.Conf
               ,toggleSharpAH :: AddHandler ()
               }

globalEnv :: IORef Env
globalEnv = unsafePerformIO $ newIORef undefined

get :: (Env -> a) -> a
get f = unsafePerformIO $ f <$> readIORef globalEnv

getSetting :: Read a => Settings.Setting a -> IO a
getSetting s = do
  c <- readTVarIO (get conf)
  return $ Settings.getSetting' c s
