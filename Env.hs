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

data Widgets = Widgets
  {window :: Window
  ,boardCanvas, captureCanvas, treeCanvas :: DrawingArea
  ,gameLabel, topPlayer, bottomPlayer, gameClock, topClock, bottomClock, statusLabel, harlogLabel, moveLabel, topUsedClock, bottomUsedClock :: Label
  ,setupGrid, captureGrid, settingsGrid, colourGrid :: Grid
  ,myGamesItem, openGamesItem, watchGamesItem, recentGamesItem, viewGameItem, playBotItem, flipBoard :: MenuItem
  ,blindModeMenu :: Menu
  ,settingsItem :: MenuItem
  ,settingsDialog :: Dialog
  ,treeGrid, mainGrid :: Grid
  ,treeScrolledWindow :: ScrolledWindow
  ,actionColumn, accelColumn :: TreeViewColumn
  ,keyTreeView :: TreeView
  ,dialogActionArea :: ButtonBox
  ,menuBar :: MenuBar
  ,gameMenu :: Menu
  ,gameMenuItem :: MenuItem
  ,buttonGrid, gameGrid :: Grid
  ,sendButton, resignButton :: Button
  }

mkWidgetGetter ''Widgets "getWidgets"

data Env = Env
  {widgets :: Widgets
  ,icons :: Map.Map PieceSet (Array (Colour, Int) Surface)
  ,setupLabels :: [Label]
  ,setDrawBoard :: (DrawingArea -> Render ()) -> IO ()
  ,setDrawSetupIcons :: [(DrawingArea -> Render ()) -> IO ()]
  ,setDrawCapture :: (DrawingArea -> Render ()) -> IO ()
  ,setDrawTree :: (DrawingArea -> Render ()) -> IO ()
  ,botLadderBotsRef :: TVar (IO [BotLadderBot])
  ,statusStack :: TVar [(Unique, String)]
  ,myGames :: TVar [Protocol.GameInfo]
  ,openGames :: TVar [Protocol.GameInfo]
  ,liveGames :: TVar [LiveGameInfo]
  ,postalGames :: TVar [LiveGameInfo]
  ,conf :: TVar Conf
  ,gameroomRef :: TVar (Maybe Gameroom)
  ,setConf :: Conf -> IO ()
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
