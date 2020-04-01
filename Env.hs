{-# LANGUAGE LambdaCase, TemplateHaskell, ScopedTypeVariables, DataKinds #-}

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
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map

import qualified Protocol
import Protocol (arimaaPost, Gameroom, PlayInfo, getFields, GameInfo, reserveSeat, sit)
import Scrape
import Base
import Templates
import Misc

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
  ,setupGrid, captureGrid, settingsGrid, colourGrid :: Grid
  ,myGamesItem, openGamesItem, watchGamesItem, viewGameItem, playBotItem, flipBoard :: MenuItem
  ,blindModeMenu :: Menu
  ,settingsItem :: MenuItem
  ,settingsDialog :: Dialog
  ,treeGrid, mainGrid :: Grid
  ,treeScrolledWindow :: ScrolledWindow
  ,actionColumn, accelColumn :: TreeViewColumn
  ,keyTreeView :: TreeView
  ,dialogActionArea :: ButtonBox
  }

mkWidgetGetter ''Widgets "getWidgets"

data Env = Env
  {buttonSet :: ButtonSet
  ,widgets :: Widgets
  ,icons :: Map.Map PieceSet (Array (Colour, Int) Surface)
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
  ,toggleSharpAH :: AddHandler ()
  ,widgetsToConf :: IO (Conf -> Conf)
  ,confToWidgets :: Conf -> IO ()
  ,trapMask :: Surface
  }

globalEnv :: IORef Env
globalEnv = unsafePerformIO $ newIORef undefined

get :: (Env -> a) -> a
get f = unsafePerformIO $ f <$> readIORef globalEnv

getConf' :: Read a => Setting a -> IO a
getConf' s = do
  c <- readTVarIO (get conf)
  return $ getSetting' c s

getConf :: Read a => Setting a -> a
getConf = unsafePerformIO . getConf'

settingsPlace = AutoFromAppName "nosteps"

-- disambig necessary
saveSettings :: IO ()
saveSettings = readTVarIO (get conf) >>= Data.AppSettings.saveSettings emptyDefaultConfig settingsPlace
