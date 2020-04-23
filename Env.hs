{-# LANGUAGE TemplateHaskell, ImplicitParams #-}

module Env where

import Graphics.UI.Gtk hiding (get)
import Data.Array.IArray
import Graphics.Rendering.Cairo
import Control.Concurrent.STM
import Data.Unique
import Data.AppSettings
import qualified Data.Map.Strict as Map

import Protocol (Gameroom, GameInfo)
import Scrape
import Base (Colour)
import Templates
import Misc (PieceSet)

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
  ,myGames :: TVar [GameInfo]
  ,openGames :: TVar [GameInfo]
  ,liveGames :: TVar [ScrapeGameInfo]
  ,postalGames :: TVar [ScrapeGameInfo]
  ,conf :: TVar Conf
  ,gameroomRef :: TVar (Maybe Gameroom)
  ,setConf :: Conf -> IO ()
  ,widgetsToConf :: IO (Conf -> Conf)
  ,confToWidgets :: Conf -> IO ()
  ,trapMask :: Surface
  }

get :: (?env :: Env) => (Env -> a) -> a
get f = f ?env

getConf :: (?env :: Env, Read a) => Setting a -> IO a
getConf s = do
  c <- readTVarIO (get conf)
  return $ getSetting' c s
