{-# LANGUAGE LambdaCase #-}

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

import qualified Protocol
import Protocol (arimaaPost, Gameroom, PlayInfo, getFields, GameInfo, reserveSeat, sit)
import Scrape
import Base
import Notation

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
               ,conf :: TVar Conf
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
               ,killPlansButton :: CheckButton
               ,setConf :: Conf -> IO ()
               ,confAH :: AddHandler Conf
               ,initialConf :: Conf
               ,longNotationButton, compressedNotationButton, silvermittNotationButton :: RadioButton
               ,noPlansButton, plansButton, premovesButton :: RadioButton
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
enablePremoves = Setting "enable-premoves" True

data PlanSetting = NoPlans | EnablePlans | EnablePremoves deriving (Show, Read, Eq)

planSetting = Setting "plans" EnablePremoves

notation = Setting "notation" Compressed

currentColour, viewColour, premoveColour, liveTrapColour, goldColour, silverColour
  ,invisibleArrowColour, goldArrowColour, silverArrowColour, multipleArrowColour
  ,illegalArrowColour, premoveArrowColour :: Setting (Double, Double, Double)

currentColour = Setting "current-colour" (0, 0.7, 0)
viewColour = Setting "view-colour" (0.9, 0, 0)
premoveColour = Setting "premove-colour" (0.8, 0, 0.8)
liveTrapColour = Setting "live-trap-colour" (1, 0, 0)
goldColour = Setting "gold-colour" (0.9, 0.7, 0)
silverColour = Setting "silver-colour" (0.6, 0.6, 0.8)

invisibleArrowColour = Setting "invisible-arrow-colour" (0, 0.9, 0)
goldArrowColour = Setting "gold-arrow-colour" (1, 0, 0)
silverArrowColour = Setting "silver-arrow-colour" (0, 0, 1)
multipleArrowColour = Setting "multiple-arrow-colour" (0.8, 0.6, 0)
illegalArrowColour = Setting "illegal-arrow-colour" (0, 0, 0)
premoveArrowColour = Setting "premove-arrow-colour" (0.8, 0, 0.8)

plansEnabled, premovesEnabled :: Conf -> Bool
plansEnabled conf = case getSetting' conf planSetting of
  NoPlans -> False
  _ -> True

premovesEnabled conf = case getSetting' conf planSetting of
  EnablePremoves -> True
  _ -> False

----------------------------------------------------------------

defaultSettings = getDefaultConfig $ do
  setting currentColour
  setting viewColour
  setting premoveColour
  setting liveTrapColour
  setting goldColour
  setting silverColour

  setting invisibleArrowColour
  setting goldArrowColour
  setting silverArrowColour
  setting multipleArrowColour
  setting illegalArrowColour
  setting premoveArrowColour
  
saveSettings :: IO ()
saveSettings = readTVarIO (get conf) >>= Data.AppSettings.saveSettings defaultSettings settingsPlace
