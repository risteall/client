{-# LANGUAGE TemplateHaskell, ImplicitParams, StandaloneDeriving, TupleSections, DeriveLift #-}

module Env where

import Graphics.UI.Gtk hiding (get)
import Data.Array.IArray
import Graphics.Rendering.Cairo
import Control.Concurrent.STM
import Data.Unique
import Data.AppSettings
import qualified Data.Map.Strict as Map
import Control.Concurrent.MVar
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Reactive.Banana.Frameworks

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
  ,myGames :: MVar [GameInfo]
  ,openGames :: MVar [GameInfo]
  ,liveGames :: MVar [ScrapeGameInfo]
  ,postalGames :: MVar [ScrapeGameInfo]
  ,recentGames :: MVar [ScrapeGameInfo]
  ,conf :: TVar Conf
  ,gameroomRef :: TVar (Maybe Gameroom)
  ,setConf :: Conf -> IO ()
  ,trapMask :: Surface
  }

get :: (?env :: Env) => (Env -> a) -> a
get f = f ?env

getConf :: (?env :: Env, Read a) => Setting a -> IO a
getConf s = do
  c <- readTVarIO (get conf)
  return $ getSetting' c s

----------------------------------------------------------------

-- defined here because needs Env, used in Main

deriving instance Lift Modifier

declareKeys :: [(String, [Modifier], String, String, ExpQ)] -> Q [Dec]
declareKeys l = do
    dataDec <- dataD
      (return [])
      keysDN
      [PlainTV tv]
      Nothing
      [recC keysDN (map (\a -> (a, Bang NoSourceUnpackedness NoSourceStrictness,) <$> varT tv) names)]
      [derivClause Nothing [[t|Functor|], [t|Foldable|], [t|Traversable|]]]
    funDec <- funD keysN [clause [] (normalB (recConE keysDN $ zipWith f names l)) []]
    sig <- sigD keysN (forallT [] (cxt [[t|?env :: Env|]])
                                  (conT keysDN `appT` [t|(Setting ([Modifier], KeyVal), String, Maybe (Widgets -> AddHandler ()))|]))
    return [dataDec, sig, funDec]
  where
    names = map (\(a,_,_,_,_) -> mkName a) l
    settingName a = uncamel (init a) ++ "-key"
    keysDN = mkName "Keys"
    tv = mkName "a"
    keysN = mkName "keys"
    f n (a,b,c,d,e) = do
      expr <- tupE [[|Setting|]
                    `appE` stringE (settingName a)
                    `appE` tupE [lift b, [|keyFromName . fromString|] `appE` stringE c]
                   ,stringE d
                   ,e
                   ]
      return (n, expr)
