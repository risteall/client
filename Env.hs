{-# LANGUAGE TemplateHaskell, ImplicitParams, DeriveLift #-}

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

--mkWidgetGetter ''Widgets "getWidgets"

getWidgets :: Builder -> IO Widgets
getWidgets b_ax4J
  = do window_ax4K <- ((builderGetObject b_ax4J) castToWindow)
                        "window"
       boardCanvas_ax4L <- ((builderGetObject b_ax4J) castToDrawingArea)
                             "board-canvas"
       captureCanvas_ax4M <- ((builderGetObject b_ax4J) castToDrawingArea)
                               "capture-canvas"
       treeCanvas_ax4N <- ((builderGetObject b_ax4J) castToDrawingArea)
                            "tree-canvas"
       gameLabel_ax4O <- ((builderGetObject b_ax4J) castToLabel)
                           "game-label"
       topPlayer_ax4P <- ((builderGetObject b_ax4J) castToLabel)
                           "top-player"
       bottomPlayer_ax4Q <- ((builderGetObject b_ax4J) castToLabel)
                              "bottom-player"
       gameClock_ax4R <- ((builderGetObject b_ax4J) castToLabel)
                           "game-clock"
       topClock_ax4S <- ((builderGetObject b_ax4J) castToLabel)
                          "top-clock"
       bottomClock_ax4T <- ((builderGetObject b_ax4J) castToLabel)
                             "bottom-clock"
       statusLabel_ax4U <- ((builderGetObject b_ax4J) castToLabel)
                             "status-label"
       harlogLabel_ax4V <- ((builderGetObject b_ax4J) castToLabel)
                             "harlog-label"
       moveLabel_ax4W <- ((builderGetObject b_ax4J) castToLabel)
                           "move-label"
       topUsedClock_ax4X <- ((builderGetObject b_ax4J) castToLabel)
                              "top-used-clock"
       bottomUsedClock_ax4Y <- ((builderGetObject b_ax4J) castToLabel)
                                 "bottom-used-clock"
       setupGrid_ax4Z <- ((builderGetObject b_ax4J) castToGrid)
                           "setup-grid"
       captureGrid_ax50 <- ((builderGetObject b_ax4J) castToGrid)
                             "capture-grid"
       settingsGrid_ax51 <- ((builderGetObject b_ax4J) castToGrid)
                              "settings-grid"
       colourGrid_ax52 <- ((builderGetObject b_ax4J) castToGrid)
                            "colour-grid"
       myGamesItem_ax53 <- ((builderGetObject b_ax4J) castToMenuItem)
                             "my-games-item"
       openGamesItem_ax54 <- ((builderGetObject b_ax4J) castToMenuItem)
                               "open-games-item"
       watchGamesItem_ax55 <- ((builderGetObject b_ax4J) castToMenuItem)
                                "watch-games-item"
       recentGamesItem_ax56 <- ((builderGetObject b_ax4J) castToMenuItem)
                                 "recent-games-item"
       viewGameItem_ax57 <- ((builderGetObject b_ax4J) castToMenuItem)
                              "view-game-item"
       playBotItem_ax58 <- ((builderGetObject b_ax4J) castToMenuItem)
                             "play-bot-item"
       flipBoard_ax59 <- ((builderGetObject b_ax4J) castToMenuItem)
                           "flip-board"
       blindModeMenu_ax5a <- ((builderGetObject b_ax4J) castToMenu)
                               "blind-mode-menu"
       settingsItem_ax5b <- ((builderGetObject b_ax4J) castToMenuItem)
                              "settings-item"
       settingsDialog_ax5c <- ((builderGetObject b_ax4J) castToDialog)
                                "settings-dialog"
       treeGrid_ax5d <- ((builderGetObject b_ax4J) castToGrid) "tree-grid"
       mainGrid_ax5e <- ((builderGetObject b_ax4J) castToGrid) "main-grid"
       treeScrolledWindow_ax5f <- ((builderGetObject b_ax4J)
                                     castToScrolledWindow)
                                    "tree-scrolled-window"
       actionColumn_ax5g <- ((builderGetObject b_ax4J)
                               castToTreeViewColumn)
                              "action-column"
       accelColumn_ax5h <- ((builderGetObject b_ax4J)
                              castToTreeViewColumn)
                             "accel-column"
       keyTreeView_ax5i <- ((builderGetObject b_ax4J) castToTreeView)
                             "key-tree-view"
       dialogActionArea_ax5j <- ((builderGetObject b_ax4J)
                                   castToButtonBox)
                                  "dialog-action-area"
       menuBar_ax5k <- ((builderGetObject b_ax4J) castToMenuBar)
                         "menu-bar"
       gameMenu_ax5l <- ((builderGetObject b_ax4J) castToMenu) "game-menu"
       gameMenuItem_ax5m <- ((builderGetObject b_ax4J) castToMenuItem)
                              "game-menu-item"
       buttonGrid_ax5n <- ((builderGetObject b_ax4J) castToGrid)
                            "button-grid"
       gameGrid_ax5o <- ((builderGetObject b_ax4J) castToGrid) "game-grid"
       sendButton_ax5p <- ((builderGetObject b_ax4J) castToButton)
                            "send-button"
       resignButton_ax5q <- ((builderGetObject b_ax4J) castToButton)
                              "resign-button"
       return
         Widgets
           {window = window_ax4K, boardCanvas = boardCanvas_ax4L,
            captureCanvas = captureCanvas_ax4M, treeCanvas = treeCanvas_ax4N,
            gameLabel = gameLabel_ax4O, topPlayer = topPlayer_ax4P,
            bottomPlayer = bottomPlayer_ax4Q, gameClock = gameClock_ax4R,
            topClock = topClock_ax4S, bottomClock = bottomClock_ax4T,
            statusLabel = statusLabel_ax4U, harlogLabel = harlogLabel_ax4V,
            moveLabel = moveLabel_ax4W, topUsedClock = topUsedClock_ax4X,
            bottomUsedClock = bottomUsedClock_ax4Y, setupGrid = setupGrid_ax4Z,
            captureGrid = captureGrid_ax50, settingsGrid = settingsGrid_ax51,
            colourGrid = colourGrid_ax52, myGamesItem = myGamesItem_ax53,
            openGamesItem = openGamesItem_ax54,
            watchGamesItem = watchGamesItem_ax55,
            recentGamesItem = recentGamesItem_ax56,
            viewGameItem = viewGameItem_ax57, playBotItem = playBotItem_ax58,
            flipBoard = flipBoard_ax59, blindModeMenu = blindModeMenu_ax5a,
            settingsItem = settingsItem_ax5b,
            settingsDialog = settingsDialog_ax5c, treeGrid = treeGrid_ax5d,
            mainGrid = mainGrid_ax5e,
            treeScrolledWindow = treeScrolledWindow_ax5f,
            actionColumn = actionColumn_ax5g, accelColumn = accelColumn_ax5h,
            keyTreeView = keyTreeView_ax5i,
            dialogActionArea = dialogActionArea_ax5j, menuBar = menuBar_ax5k,
            gameMenu = gameMenu_ax5l, gameMenuItem = gameMenuItem_ax5m,
            buttonGrid = buttonGrid_ax5n, gameGrid = gameGrid_ax5o,
            sendButton = sendButton_ax5p, resignButton = resignButton_ax5q}

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

-- deriving instance Lift Modifier

-- declareKeys :: [(String, [Modifier], String, String, ExpQ)] -> Q [Dec]
-- declareKeys l = do
--     dataDec <- dataD
--       (return [])
--       keysDN
--       [PlainTV tv]
--       Nothing
--       [recC keysDN (map (\a -> (a, Bang NoSourceUnpackedness NoSourceStrictness,) <$> varT tv) names)]
--       [derivClause Nothing [[t|Functor|], [t|Foldable|], [t|Traversable|]]]
--     funDec <- funD keysN [clause [] (normalB (recConE keysDN $ zipWith f names l)) []]
--     sig <- sigD keysN (forallT [] (cxt [[t|?env :: Env|]])
--                                   (conT keysDN `appT` [t|(Setting ([Modifier], KeyVal), String, Maybe (Widgets -> AddHandler ()))|]))
--     return [dataDec, sig, funDec]
--   where
--     names = map (\(a,_,_,_,_) -> mkName a) l
--     settingName a = uncamel (init a) ++ "-key"
--     keysDN = mkName "Keys"
--     tv = mkName "a"
--     keysN = mkName "keys"
--     f n (a,b,c,d,e) = do
--       expr <- tupE [[|Setting|]
--                     `appE` stringE (settingName a)
--                     `appE` tupE [lift b, [|keyFromName . fromString|] `appE` stringE c]
--                    ,stringE d
--                    ,e
--                    ]
--       return (n, expr)
