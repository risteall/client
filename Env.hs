{-# LANGUAGE LambdaCase #-}

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

import qualified Protocol
import Protocol (arimaaPost, Gameroom, PlayInfo, getFields, GameInfo, reserveSeat, sit)
import Scrape
import Base

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
