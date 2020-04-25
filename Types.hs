{-# LANGUAGE TemplateHaskell, GADTs #-}

module Types where

import qualified Rank2
import qualified Rank2.TH
import Data.Tree(Forest)
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Array.IArray
import Data.AppSettings
import Data.Maybe
import Text.Printf

import Base
import Node hiding (position)

-- per-game constants, as seen by the event-handling function
data GameParams = GameParams
  {names :: Array Colour String
  ,ratings :: Array Colour (Maybe Int)
  ,isUser :: Array Colour Bool
  ,timeControl :: TimeControl
  ,rated :: Bool
  }

emptyGameParams :: GameParams
emptyGameParams = GameParams (colourArray (repeat ""))
                             (colourArray (repeat Nothing))
                             (colourArray (repeat False))
                             (fromJust (parseTimeControl "1d/30d/100/0/10m/0"))
                             False

---------------------------------------------------------------

data Request = RequestStart | RequestMove GenMove | RequestResign Colour

data Update = UpdateStart
            | UpdateMove (GenMove, Maybe Int)
            | UpdateResult (Colour, Reason)
            | UpdateClock (Colour, Int)
            | UpdateUsed {playerUsed :: Maybe (Colour, Int), gameUsed :: Maybe Int, timeDiff :: Maybe Int}
--            | UpdateUsed (Colour, Int)
  --          | UpdateGameUsed Int
              deriving Show

data GameState = GameState
  {started :: Bool
  ,position :: Position
  ,result :: Maybe (Colour, Reason)
  } deriving Show

newGameState = GameState{started = False, position = newPosition, result = Nothing}

updateGameState gs UpdateStart = gs{started = True}
updateGameState gs (UpdateMove (m,_)) = either (error . printf "Illegal move from server (%s)")
                                               (\p -> gs{position = p})
                                               $ playGenMove (position gs) m
updateGameState gs (UpdateResult x) = gs{result = Just x}
updateGameState gs _ = gs

----------------------------------------------------------------

data NewGame = forall a. Eq a => NewGame
  {params :: GameParams
  ,initialTree :: Forest SomeNode
  ,request :: Either (Request -> IO ()) (Request -> IO a, MomentIO (Event a))
  ,updates :: MomentIO (Behavior GameState, Event Update)
  ,cleanup :: IO ()
  }

data Events f = Events
  {newGameE :: f NewGame
  ,leftPress :: f (Square, (Double, Double))
  ,rightPress :: f Square
  ,release :: f Square
  ,motion :: f Square
  ,flipE :: f ()
  ,tick :: f ()
  ,blindMode :: f (Bool, Bool)
  ,setupIconsE :: [f ()]
  ,treePress :: f (Double, Double)
  ,sendE :: f ()
  ,resignE :: f ()
  ,sharpE :: f ()
  ,planE :: f ()
  ,clearE :: f ()
  ,prevE :: f ()
  ,nextE :: f ()
  ,startE :: f ()
  ,endE :: f ()
  ,currentE :: f ()
  ,deleteNodeE :: f ()
  ,deleteLineE :: f ()
  ,deleteAllE :: f ()
  ,prevBranchE :: f ()
  ,nextBranchE :: f ()
  ,deleteFromHereE :: f ()
  ,confE :: f Conf
  ,toggleSharpE :: f ()
  ,copyMovelistE :: f ()
  }

Rank2.TH.deriveFunctor ''Events
Rank2.TH.deriveFoldable ''Events
Rank2.TH.deriveTraversable ''Events
