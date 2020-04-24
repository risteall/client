-- -*- Haskell -*-

{-# LANGUAGE LambdaCase, TupleSections, ScopedTypeVariables, NamedFieldPuns, MultiWayIf, PatternGuards, RecursiveDo, DeriveGeneric, DeriveAnyClass, RecordWildCards, StandaloneDeriving, CPP, DataKinds, TypeApplications, DeriveTraversable, DeriveLift, TemplateHaskell, ImplicitParams, FlexibleInstances, RankNTypes #-}

import Data.Array.IArray
import Graphics.UI.Gtk hiding (get, set, Shift, Arrow, rectangle, on)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(Render), runRender)
import Data.IORef
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List.Split
import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent
import Data.Bifunctor
import Data.Tree hiding (drawTree)
import Text.Read hiding (lift, get)
import Network.HTTP hiding (Request, password)
import Text.Regex
import Data.Time.Clock.POSIX
import Control.Monad
import Control.Concurrent.Async
import Control.Exception
import Reactive.Banana hiding (split)
import qualified Reactive.Banana as RB
import Reactive.Banana.Frameworks
import GHC.Exts (fromString)
import Text.Printf
import Data.Unique
import Data.AppSettings
import Lens.Micro
import System.Random
import System.Timeout
import Control.Monad.Trans.Reader
import System.IO
import Data.Foldable

import Debug.Trace

import Draw
import qualified Protocol
import Protocol (arimaaPost, Gameroom, PlayInfo, getFields, GameInfo, reserveSeat, sit)
import Base
import GameTree
import Scrape
import qualified Node
import EventNetwork
import Env
import Sharp
import WidgetValue
import Settings
import Misc
import Templates

#ifndef LOCAL
import Paths_nosteps
#endif

----------------------------------------------------------------

deriving instance Read Modifier

declareKeys
  [("sendE", [], "s", "Send move", [|Just ((`onS` buttonActivated) . sendButton)|])
  ,("resignE", [Gtk.Control], "r", "Resign", [|Just ((`onS` buttonActivated) . resignButton)|])
  ,("planE", [], "space", "Enter plan move", [|Nothing|])
  ,("sharpE", [], "x", "Run Sharp", [|Nothing|])
  ,("toggleSharpE", [], "p", "Pause and unpause Sharp", [|Nothing|])
  ,("clearE", [], "Escape", "Clear arrows", [|Nothing|])
  ,("prevE", [], "Up", "Previous move", [|Nothing|])
  ,("nextE", [], "Down", "Next move", [|Nothing|])
  ,("startE", [Gtk.Control], "Up", "Go to game start", [|Nothing|])
  ,("endE", [Gtk.Control], "Down", "Go to game end", [|Nothing|])
  ,("currentE", [], "c", "Go to current game position", [|Nothing|])
  ,("prevBranchE", [], "Left", "Previous variation", [|Nothing|])
  ,("nextBranchE", [], "Right", "Next variation", [|Nothing|])
  ,("deleteNodeE", [], "BackSpace", "Remove plan move", [|Nothing|])
  ,("deleteLineE", [Gtk.Control], "BackSpace", "Remove plan variation (back to last branch)", [|Nothing|])
  ,("deleteAllE", [Gtk.Control, Gtk.Shift], "BackSpace", "Remove all plans", [|Nothing|])
  ,("deleteFromHereE", [], "Delete", "Remove plans starting at current position", [|Nothing|])
  ,("copyMovelistE", [Gtk.Control], "c", "Copy movelist", [|Nothing|])
  ,("toggleFullscreenE", [], "F11", "Toggle fullscreen", [|Nothing|])
  ,("dummyGameE", [Gtk.Control], "d", "Dummy game", [|Nothing|])
  ]

anyM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
anyM f = foldr g (return False)
  where g x y = f x >>= \case
          True -> return True
          False -> y

initKeyActions :: (?env :: Env) => Widgets -> IO (Keys (AddHandler ()))
initKeyActions widgets = do
  l <- mapM (\(s,_,mf) -> (s,mf,) <$> newAddHandler) keys
  forM_ l $ \(_, mf, (_, fire)) -> forM_ mf $ \f -> register (f widgets) fire
  window widgets `on` keyPressEvent $ do
    k <- eventKeyVal
    m <- eventModifier
    let
      f (s, _, (_, fire)) = do
        (m', k') <- getConf s
        if k == k' && elem m (permutations m')
          then do {fire (); return True}
          else return False
    liftIO $ anyM f l
  return $ fmap (\(_,_,(ah,_)) -> ah) l

----------------------------------------------------------------

errorMessage :: (?env :: Env) => String -> IO ()
errorMessage s = do
  hPutStrLn stderr s
  void $ setStatus ("<span weight=\"bold\" foreground=\"#0d0\">" ++ s ++ "</span>") (Just 15)

defaultHandler :: (?env :: Env) => a -> IO a -> IO a
defaultHandler x = flip catch $ \(e :: SomeException) -> do
  errorMessage (show e)
  return x

class WrapCallback c where
  withHandler :: (?env :: Env) => c -> c

instance WrapCallback (IO ()) where
  withHandler = defaultHandler ()

instance WrapCallback (EventM t Bool) where
  withHandler c = ReaderT $ \env -> defaultHandler True (runReaderT c env)
           -- withRunInIO $ \run -> defaultHandler True (run e)

instance WrapCallback (Render ()) where
  withHandler c = Render $ ReaderT $ \env -> defaultHandler () (runReaderT (runRender c) env)

instance WrapCallback (r -> IO ()) where
  withHandler c = \env -> defaultHandler () (c env)

on :: (?env :: Env, WrapCallback callback) => object -> Signal object callback -> callback -> IO (ConnectId object)
on obj s c = Gtk.on obj s (withHandler c)

----------------------------------------------------------------

onS :: (?env :: Env, GObjectClass object) => object -> Signal object (IO ()) -> AddHandler ()
onS o s = AddHandler $ \h -> do
  c <- on o s (h ())
  return $ signalDisconnect c

onE :: (?env :: Env, GObjectClass object) => object -> Signal object (EventM t Bool) -> EventM t (Maybe a) -> AddHandler a
onE o s k = AddHandler $ \h -> do
  c <- on o s $ k >>= \case
    Nothing -> return False
    Just a -> do
      liftIO $ h a
      return True
  return $ signalDisconnect c

----------------------------------------------------------------

botLadderBots :: (?env :: Env) => IO [BotLadderBot]
botLadderBots = join $ readTVarIO (get botLadderBotsRef)

gameroom :: (?env :: Env) => IO Gameroom
gameroom = readTVarIO (get gameroomRef) >>= \case
  Just g -> return g
  Nothing -> do
    u <- getConf username
    p <- getConf password
    case (u, p) of
      (Just u'@(_:_), Just p'@(_:_)) -> Protocol.login u' p'
      _ -> error "Can't get gameroom"

----------------------------------------------------------------

setUsernameAndPassword :: (?env :: Env) => String -> String -> IO ()
setUsernameAndPassword u p = do
  c <- readTVarIO (get conf)
  get setConf $ setSetting (setSetting c username (Just u)) password (Just p)
  atomically $ writeTVar (get gameroomRef) Nothing   -- bad: should logout
  getBotLadder

----------------------------------------------------------------
  
-- dialogValue :: WidgetValue a b => String -> (a -> IO c) -> IO c
-- dialogValue label f = do
--   d <- newDialog
--   Gtk.set d [windowTransientFor := get window]
--   dialogAddButton d "Cancel" ResponseCancel
--   widgetGrabDefault =<< dialogAddButton d label ResponseOk
--   w <- makeWidget (defaultValue :: a)
--   u <- castToContainer <$> dialogGetContentArea d
--   containerAdd u w
--   widgetShowAll d
--   d `on` response $ \case
--     ResponseOk -> getValue w >>= \case
--       Just x -> do
--         widgetHide d
--         f x
--       Nothing -> return ()
--     _ -> widgetHide d

----------------------------------------------------------------

background :: IO a -> (a -> IO ()) -> IO ()
background x y = void $ forkIO $ x >>= postGUIAsync . y

showStatus :: (?env :: Env) => IO ()
showStatus = do
  readTVarIO (get statusStack) >>= \case
    [] -> labelSetMarkup (get (statusLabel . widgets)) ""
    (_,m):_ -> labelSetMarkup (get (statusLabel . widgets)) m

setStatus :: (?env :: Env) => String -> Maybe Int -> IO (IO ())
setStatus message timeout = do
  u <- newUnique
  atomically $ modifyTVar (get statusStack) $ ((u, message) :)
  postGUIAsync showStatus
  let remove = do
        atomically $ modifyTVar (get statusStack) $ filter ((/= u) . fst)
        postGUIAsync showStatus
  forM_ timeout $ \n -> void $ forkIO $ do
    threadDelay (n * 1000000)
    remove
  return remove

withStatus :: (?env :: Env) => String -> IO a -> IO a
withStatus message action = do
  remove <- setStatus message Nothing
  finally action remove

----------------------------------------------------------------

sendMessage :: TChan (a, Int) -> TVar Int -> a -> IO Int
sendMessage ch r m = atomically $ do
  n <- readTVar r
  writeTChan ch (m, n)
  writeTVar r (n+1)
  return n

toServer :: (?env :: Env) => PlayInfo -> String -> TChan (Request, Int) -> TChan Int -> IO ()
toServer (gsurl, sid) auth requestChan responseChan = forever $ try f >>= \case
    Left (Protocol.ServerError e) -> errorMessage e
    Right _ -> return ()
  where
    f = bracket
          (atomically (readTChan requestChan))
          (\(_, q) -> atomically (writeTChan responseChan q))
          (\(request, _) -> case request of     
            RequestStart -> void $ arimaaPost gsurl [("action", "startgame"), ("sid", sid), ("auth", auth)]
            RequestMove move -> void $ arimaaPost gsurl [("action", "move"), ("sid", sid), ("auth", auth), ("move", showGenMove move)]
            RequestResign _ -> void $ arimaaPost gsurl [("action", "resign"), ("sid", sid), ("auth", auth)]
          )

getUpdates :: [(String, String)] -> Bool -> TChan Update -> IO (Bool, Bool)
getUpdates response started updateChan = do
--  forM_ response $ uncurry $ printf "%s: %s\n"
--  putStrLn $ replicate 64 '-'
  
  let send = atomically . writeTChan updateChan
  
  let start = not started && isJust (lookup "starttime" response)
  when start $ send UpdateStart

  forM_ (lookup "moves" response) $ \m -> let moves = mapMaybe readGenMove $ splitOn "\DC3" m in
    when (not (null moves)) $ mapM_ (send . UpdateMove)
                                    $ map (,Nothing) (init moves) ++ [(last moves, read <$> lookup "lastmoveused" response)]

  finished <- case lookup "result" response of
    Just [c1,c2] | Just r <- readReason c2 -> do
      send $ UpdateResult $ (if elem c1 "wg" then Gold else Silver, r)
      return True
    _ -> do

      forM_ [(Gold, "tcwreserve2"), (Silver, "tcbreserve2")] $ \(c, s) ->
        forM_ (lookup s response >>= readMaybe) $ \t ->
          send $ UpdateClock (c, t)

      let playerUsed = case lookup "turn" response of
            Just [c] | Just player <- charToColour c
                     , Just s <- lookup (colourToServerChar player : "used") response
                     , Just t <- readMaybe s
                       -> Just (player, t)
            _ -> Nothing

      let [gameUsed, t] = map (\s -> lookup s response >>= readMaybe )
                              ["tcgamenow", "timeonserver"]

      t' <- truncate <$> getPOSIXTime
      send $ UpdateUsed {playerUsed, gameUsed, timeDiff = (t' -) <$> t}

      return False

  return (started || start, finished)

fromServer :: (?env :: Env) => PlayInfo -> [(String, String)] -> Bool -> TChan Update -> IO ()
fromServer (gsurl, sid) response started updateChan = do
  let [lc, ml, cl] = getFields ["lastchange", "moveslength", "chatlength"] response
  
  response' <- handle (\(Protocol.ServerError s) -> do {errorMessage s; return response})
                 $ arimaaPost gsurl [("action", "updategamestate")
                                    ,("sid", sid)
                                    ,("wait", "1")
                                    ,("lastchange", lc)
                                    ,("moveslength", ml)
                                    ,("chatlength", cl)
                                    ]

  (started, finished) <- getUpdates response' started updateChan
  when (not finished) $ fromServer (gsurl, sid) response' started updateChan

channelEvent :: TChan a -> MomentIO (Event a)
channelEvent chan = do
  ah <- liftIO $ do
    (ah, fire) <- newAddHandler
    let f = atomically (tryReadTChan chan) >>= maybe (return ()) (\x -> do {fire x; f})
    timeoutAdd (True <$ f) 100
    return ah
  fromAddHandler ah

setServerGame :: (?env :: Env) => GameInfo -> IO ()
setServerGame gameInfo = handle (\(Protocol.ServerError s) -> errorMessage s) $ do
  gameroom <- gameroom
  ri <- reserveSeat gameroom gameInfo
  ((gsurl, sid), _, _) <- sit gameroom ri

  requestChan <- newTChanIO
  responseChan <- newTChanIO
  updateChan <- newTChanIO

  nextId <- newTVarIO 0

  response <- arimaaPost gsurl [("action", "gamestate"), ("sid", sid), ("wait", "0")]

  (started, finished) <- getUpdates response False updateChan

  -- TODO: thread handling; error checking
  t1 <- forkIO $ when (not finished) $ fromServer (gsurl, sid) response started updateChan
  t2 <- forkIO $ toServer (gsurl, sid) (fromJust (lookup "auth" response)) requestChan responseChan

  postGUIAsync
    $ newGame GameParams{names = mapColourArray $ \c -> if c == Protocol.role gameInfo then "me" else Protocol.opponent gameInfo
                        ,ratings = colourArray $ map (\s -> lookup s response >>= readMaybe) ["wrating", "brating"]
                        ,isUser = mapColourArray (== Protocol.role gameInfo)
                        ,timeControl = Protocol.timecontrol gameInfo
                        ,rated = Protocol.rated gameInfo
                        }
              []
              (sendMessage requestChan nextId)
              (channelEvent responseChan)
              (do
                  e <- channelEvent updateChan
                  b <- accumB newGameState (flip updateGameState <$> e)
                  return (b, e))
              (mapM_ killThread [t1, t2])

watchGame :: (?env :: Env) => String -> IO ()
watchGame gid = handle (\(Protocol.ServerError s) -> errorMessage s) $ do
  gameroom <- gameroom
  ri <- Protocol.reserveView gameroom gid
  ((gsurl, sid), _, _) <- sit gameroom ri

  updateChan <- newTChanIO

  response <- arimaaPost gsurl [("action", "gamestate"), ("sid", sid), ("wait", "0")]

  (started, finished) <- getUpdates response False updateChan

  -- TODO: thread handling; error checking
  t1 <- forkIO $ when (not finished) $ fromServer (gsurl, sid) response started updateChan

  let tc = fromMaybe (fromJust (parseTimeControl "0/0")) $ do
        s <- lookup "timecontrol" response
        [_, s'] <- matchRegex (mkRegex "^(. )?(.*)") s
        parseTimeControl s'

  postGUIAsync
                   -- TODO: handle missing values
                   -- TODO: strip * from player names
    $ newGame' GameParams{names = colourArray $ map (\s -> fromJust (lookup s response)) ["wplayer", "bplayer"]
                        ,ratings = colourArray $ map (\s -> lookup s response >>= readMaybe) ["wrating", "brating"]
                        ,isUser = mapColourArray $ const False
                        ,timeControl = tc
                        ,rated = fromJust (lookup "rated" response) == "1"
                        }
              [] (\_ -> return ())
              (do
                  e <- channelEvent updateChan
                  b <- accumB newGameState (flip updateGameState <$> e)
                  return (b, e))
              (killThread t1)

----------------------------------------------------------------

setMVar :: MVar a -> a -> IO ()
setMVar m x = do
  _ <- tryTakeMVar m
  putMVar m x

updateServerGames :: (?env :: Env) => IO ()
updateServerGames = forever $ do
  gameroom <- gameroom
  Protocol.myGames gameroom >>= setMVar (get myGames)
  Protocol.openGames gameroom >>= setMVar (get openGames)
  getGames "http://arimaa.com/arimaa/gameroom/watchgames.cgi" >>= setMVar (get liveGames)
  getGames "http://arimaa.com/arimaa/gameroom/postalgames.cgi" >>= setMVar (get postalGames)
  getGames "http://arimaa.com/arimaa/gameroom/recentgames.cgi" >>= setMVar (get recentGames)

  threadDelay (30 * 10^6)

makeTreeStore :: Forest a -> [(String, a -> [AttrOp CellRendererText])] -> IO (TreeStore a, TreeView)
makeTreeStore forest l = do
  ts <- treeStoreNew forest
  tv <- treeViewNewWithModel ts
  cr <- cellRendererTextNew
  forM_ l $ \(s, g) -> do
    col <- treeViewColumnNew
    treeViewAppendColumn tv col
    treeViewColumnSetTitle col s
    cellLayoutPackStart col cr False
    cellLayoutSetAttributes col cr ts g
  return (ts, tv)

serverGameCallback :: (?env :: Env) => [Protocol.GameInfo] -> IO ()
serverGameCallback games = do
  d <- dialogNew
  Gtk.set d [windowTransientFor := get (window . widgets)
            ,windowDefaultWidth := 400
            ,windowDefaultHeight := 400
            ]

  dialogAddButton d "Cancel" ResponseCancel
  dialogAddButton d "Open game" ResponseOk
  u <- castToContainer <$> dialogGetContentArea d

  (ts, tv) <- makeTreeStore (map (\g -> Node g []) games)
                            [("Opponent", \gi -> [cellText := Protocol.opponent gi])
                            ,("Role", \gi -> [cellText := show (Protocol.role gi)])
                            ,("Time control", \gi -> [cellText := show (Protocol.timecontrol gi)])
                            ,("Rated", \gi -> [cellText := if Protocol.rated gi then "R" else "U"])
                            ]

  treeViewExpandAll tv

  sw <- scrolledWindowNew Nothing Nothing
  Gtk.set sw [widgetVExpand := True
             ,widgetHeightRequest := 100
             ]
  containerAdd sw tv
  containerAdd u sw

  widgetShowAll d
  
  d `on` response $ \case
    ResponseOk -> treeViewGetSelection tv >>= treeSelectionGetSelected >>= \case
      Nothing -> return ()
      Just iter -> treeModelGetPath ts iter >>= treeStoreGetValue ts >>= \game -> do
        forkIO $ withStatus "Starting game" $ setServerGame game
        widgetDestroy d
    _ -> widgetDestroy d

  return ()

watchGameCallback :: (?env :: Env) => IO ()
watchGameCallback =
    background (timeout (5*10^6)
                 ((,) <$> readMVar (get liveGames) <*> readMVar (get postalGames)))
      $ maybe (return ()) $ \(liveGames, postalGames) -> do
  d <- dialogNew
  Gtk.set d [windowTransientFor := get (window . widgets)
            ,windowDefaultWidth := 600
            ,windowDefaultHeight := 500
            ]

  dialogAddButton d "Cancel" ResponseCancel
  dialogAddButton d "Watch game" ResponseOk
  u <- castToContainer <$> dialogGetContentArea d

  (ts, tv) <- makeTreeStore
      [Node (Left "Live games") (map (\g -> Node (Right g) []) liveGames)
      ,Node (Left "Postal games") (map (\g -> Node (Right g) []) postalGames)
      ]
    [("Gold", \x -> [cellText := either id (\lgi -> printf "%s (%d)" (giNames lgi ! Gold) (giRatings lgi ! Gold)) x])
    ,("Silver", \x -> [cellText := either (const "") (\lgi -> printf "%s (%d)" (giNames lgi ! Silver) (giRatings lgi ! Silver)) x])
    ,("Time control", \x -> [cellText := either (const "") (show . giTimeControl) x])
    ,("Rated", \x -> [cellText := either (const "") (\lgi -> if giRated lgi then "R" else "U") x])
    ]

  treeViewExpandAll tv

  sw <- scrolledWindowNew Nothing Nothing
  Gtk.set sw [widgetVExpand := True
             ,widgetHeightRequest := 100
             ]
  containerAdd sw tv
  containerAdd u sw

  widgetShowAll d

  d `on` response $ \case
    ResponseOk -> treeViewGetSelection tv >>= treeSelectionGetSelected >>= \case
      Nothing -> return ()
      Just iter -> treeModelGetPath ts iter >>= treeStoreGetValue ts >>= \case
        Left _ -> return ()
        Right lgi -> do
          forkIO $ withStatus "Starting game" $ watchGame $ show $ giGid lgi
          widgetDestroy d
    _ -> widgetDestroy d

  return ()

-- inefficient: TreeStore reconstructed on every call
recentGamesCallback :: (?env :: Env) => IO ()
recentGamesCallback = background (timeout (5*10^6) (readMVar (get recentGames))) $ maybe (return ()) $ \games -> do
  d <- dialogNew
  Gtk.set d [windowTransientFor := get (window . widgets)
            ,windowDefaultWidth := 800
            ,windowDefaultHeight := 500
            ]

  dialogAddButton d "Cancel" ResponseCancel
  dialogAddButton d "View" ResponseOk
  u <- castToContainer <$> dialogGetContentArea d

  (ts, tv) <- makeTreeStore
      (map (\g -> Node g []) games)
    [("Gold", \g -> [cellText := (printf "%s%s (%d)" (if giWinner g == Just Gold then "*" else "") (giNames g ! Gold) (giRatings g ! Gold) :: String)])
    ,("Silver", \g -> [cellText := (printf "%s%s (%d)" (if giWinner g == Just Silver then "*" else "") (giNames g ! Silver) (giRatings g ! Silver) :: String)])
    ,("Time control", \g -> [cellText := show (giTimeControl g)])
    ,("Rated", \g -> [cellText := if giRated g then "R" else "U"])
    ,("Reason", \g -> [cellText := maybe "" show (giReason g)])
    ,("Move count", \g -> [cellText := maybe "" show (giMoveCount g)])
    ]

  treeViewExpandAll tv

  sw <- scrolledWindowNew Nothing Nothing
  Gtk.set sw [widgetVExpand := True
             ,widgetHeightRequest := 100
             ]
  containerAdd sw tv
  containerAdd u sw

  widgetShowAll d

  d `on` response $ \case
    ResponseOk -> treeViewGetSelection tv >>= treeSelectionGetSelected >>= \case
      Nothing -> return ()
      Just iter -> treeModelGetPath ts iter >>= treeStoreGetValue ts >>= \g ->
        viewGame (giGid g) (widgetDestroy d)
    _ -> widgetDestroy d

  return ()

----------------------------------------------------------------

makeDialog' :: (?env :: Env, WidgetClass w) => w -> String -> (IO () -> IO ()) -> IO ()
makeDialog' w buttonText f = do
  d <- dialogNew
  Gtk.set d [windowTransientFor := get (window . widgets)]
  dialogAddButton d "Cancel" ResponseCancel
  widgetGrabDefault =<< dialogAddButton d buttonText ResponseOk
  u <- castToContainer <$> dialogGetContentArea d
  containerAdd u w
  widgetShowAll d
  d `on` response $ \case
    ResponseOk -> f (widgetDestroy d)
    _ -> widgetDestroy d
  return ()

makeDialog :: (?env :: Env, WidgetClass w) => w -> String -> IO Bool -> IO ()
makeDialog w buttonText f = do
  makeDialog' w buttonText $ \kill -> f >>= \case
    True -> kill
    False -> return ()

viewGameCallback :: (?env :: Env) => IO ()
viewGameCallback = do
  (w,get,_) <- labelledAccessor "Enter game id:"
  makeDialog' w "View game" $ \killDialog -> get >>= \case
    Nothing -> return ()
    Just n -> viewGame n killDialog

expandGame :: TimeControl -> [(GenMove, Maybe Int)] -> Either String [Node.Node 'Node.Regular]
expandGame tc moves = snd <$> mapAccumLM f (Nothing, Just (initialReserve tc)) moves
  where
    f :: (Maybe (Node.Node 'Node.Regular), Maybe Int) -> (GenMove, Maybe Int) -> Either String ((Maybe (Node.Node 'Node.Regular), Maybe Int), Node.Node 'Node.Regular)
    f (n, r) (m, t) = (\p -> let r' = updateReserve tc <$> t <*> r
                                 n' = Node.mkRegularNode n m p ((,) <$> t <*> r')
                             in ((Just n', r'), n'))
                        <$> playGenMove (Node.regularPosition n) m

viewGame :: (?env :: Env) => Int -> IO () -> IO ()
viewGame n killDialog = do
  background (withStatus "Fetching game" (getServerGame n)) $ \case
    Nothing -> return ()
    Just sgi -> do
      killDialog
      let nodes = either error id $ expandGame (sgiTimeControl sgi) (map (second Just) (sgiMoves sgi))
          pos = Node.regularPosition $ if null nodes then Nothing else Just (last nodes)
      newGame' GameParams{names = sgiNames sgi
                         ,ratings = Just <$> sgiRatings sgi
                         ,isUser = mapColourArray (const False)
                         ,timeControl = sgiTimeControl sgi
                         ,rated = sgiRated sgi
                         }
        (foldr (\n f -> [Node (Node.SomeNode n) f]) [] nodes)
        (\_ -> return ())
        (return (pure GameState{started = True, position = pos, result = Just $ sgiResult sgi},
                 never))
        (return ())

----------------------------------------------------------------

getBotLadder :: (?env :: Env) => IO ()
getBotLadder = do
  u <- fromMaybe "" <$> getConf username
  a <- async (botLadderAll (if null u then Nothing else Just u))
  atomically $ writeTVar (get botLadderBotsRef) $ wait a

startBot :: (?env :: Env) => String -> Colour -> IO ()
startBot url c = do
  s <- getResponseBody =<< simpleHTTP (postRequestWithBody url "application/x-www-form-urlencoded"
                                        (urlEncodeVars [("action", "player")
                                                       ,("newgame", "Start Bot")
                                                       ,("side", colourToServerChar c : [])
                                                       ]))
  when (s /= "Bot started.\n") $ errorMessage s

----------------------------------------------------------------

playBot :: (?env :: Env) => BotLadderBot -> Colour -> IO ()
playBot bot c = void $ forkIO $ do
  withStatus "Starting bot" $ startBot (botUrl bot) (flipColour c)
  gameroom <- gameroom
  openGames <- withStatus "Contacting gameroom" $ Protocol.openGames gameroom
  case find (\gi -> Protocol.opponent gi == botName bot && Protocol.role gi == c) openGames of
    Nothing -> error "Missing game"   -- TODO: something better
    Just game -> withStatus "Starting game" $ setServerGame game

playBotCallback :: (?env :: Env) => IO ()
playBotCallback = do
  d <- dialogNew
  Gtk.set d [windowTransientFor := get (window . widgets)
            ,windowDefaultWidth := 300
            ,windowDefaultHeight := 500
            ]
  dialogAddButton d "Cancel" ResponseCancel
  dialogAddButton d "Play bot" ResponseOk
  u <- castToContainer <$> dialogGetContentArea d

  l <- labelNew (Just "Play as:")
  Gtk.set l [miscXalign := 0]
  containerAdd u l

  randomButton <- radioButtonNewWithLabel "Random"
  goldButton <- radioButtonNewWithLabelFromWidget randomButton "Gold"
  silverButton <- radioButtonNewWithLabelFromWidget randomButton "Silver"
  mapM_ (containerAdd u) [randomButton, goldButton, silverButton]

  bots <- botLadderBots
  let speeds = ["Lightning", "Blitz", "Fast", "P2", "P1", "CC"]
      (rest, bits) = mapAccumL (\bs s -> partition (not . (s `isSuffixOf`) . botName) bs) bots speeds
  (ts, tv) <- makeTreeStore (zipWith (\speed bit -> Node (Left speed) (map (\bot -> Node (Right bot) []) bit))
                              (speeds ++ ["Rest"])
                              (bits ++ [rest]))
                            [("Bot", \x -> [cellText := either id botName x])
                            ,("Rating", \x -> [cellText := either (const "") (show . botRating) x])
                            ]

  sw <- scrolledWindowNew Nothing Nothing
  Gtk.set sw [widgetVExpand := True
             ,widgetHeightRequest := 100
             ]
  containerAdd sw tv
  containerAdd u sw

  widgetShowAll d
  
  d `on` response $ \case
    ResponseOk -> treeViewGetSelection tv >>= treeSelectionGetSelected >>= \case
      Nothing -> return ()
      Just iter -> treeModelGetPath ts iter >>= treeStoreGetValue ts >>= \case
        Left _ -> return ()
        Right bot -> mapM toggleButtonGetActive [randomButton, goldButton, silverButton]
                       >>= sequence . fmap fst . find snd . zip [(\b -> if b then Gold else Silver) <$> randomIO, return Gold, return Silver]
                       >>= \case
          Nothing -> return ()
          Just c -> do
            widgetDestroy d
            playBot bot c
    _ -> widgetDestroy d

  return ()

----------------------------------------------------------------

requestToUpdate RequestStart = UpdateStart
requestToUpdate (RequestMove m) = UpdateMove (m, Nothing)
requestToUpdate (RequestResign c) = UpdateResult (flipColour c, Resignation)

dummyGame :: TimeControl -> IO ()
dummyGame tc = do
  counter <- newIORef 0
  (ah, fire) <- newAddHandler
  (responseAH, fireResponse) <- newAddHandler
  newGame GameParams{names = mapColourArray (const "me")
                    ,ratings = mapColourArray (const Nothing)
                    ,isUser = mapColourArray (const True)
                    ,timeControl = tc
                    ,rated = False
                    }
    []
    (\r -> do
        fire r
        n <- readIORef counter
        modifyIORef' counter (+ 1)
        forkIO $ do
          threadDelay 1000000
          postGUIAsync $ fireResponse n
        return n
    )
    (fromAddHandler responseAH)
    (do
       e <- fromAddHandler ah
       let u = requestToUpdate <$> e
       b <- accumB newGameState (flip updateGameState <$> u)
       return (b, u)
    )
    (return ())

----------------------------------------------------------------

promptUsername :: (?env :: Env) => IO () -> IO ()
promptUsername finalAction = do
  d <- dialogNew
  Gtk.set d [windowTransientFor := get (window . widgets)]
  widgetGrabDefault =<< dialogAddButton d "OK" ResponseOk

  u <- castToContainer <$> dialogGetContentArea d

  l <- labelNew (Just "Enter gameroom stuff:")
  containerAdd u l
  [e1, e2] <- forM ["Username:", "Password:"] $ \s -> do
    b <- hBoxNew False 5
    l <- labelNew (Just s)
    e <- entryNew
    entrySetActivatesDefault e True
    containerAdd b l
    containerAdd b e
    containerAdd u b
    return e
    
  widgetShowAll d

  d `on` response $ \_ -> do
    u <- entryGetText e1
    p <- entryGetText e2
    widgetDestroy d
    setUsernameAndPassword u p
    finalAction

  return ()

initialStuff :: (?env :: Env) => IO ()
initialStuff = do
  let x = do
        getBotLadder
        void $ forkIO updateServerGames
  u <- getConf username
  p <- getConf password
  case (u, p) of
    (Just _, Just _) -> x
    _ -> promptUsername x

----------------------------------------------------------------

initKeyList :: (?env :: Env) => IO (ListStore (String, ([Modifier], KeyVal)))
initKeyList = do
  ls <- listStoreNew =<< mapM (\(setting, desc, _) -> (desc,) <$> getConf setting) (toList keys)
  treeViewSetModel (get (keyTreeView . widgets)) (Just ls)

  crt <- cellRendererTextNew
  cellLayoutPackStart (get (actionColumn . widgets)) crt False
  cellLayoutSetAttributes (get (actionColumn . widgets)) crt ls $ \(a,_) -> [cellText := a]

  cra <- cellRendererAccelNew
  Gtk.set cra [cellRendererAccelAccelMode := CellRendererAccelModeOther]

  cellLayoutPackStart (get (accelColumn . widgets)) cra False
  cellLayoutSetAttributes (get (accelColumn . widgets)) cra ls $ \(_,(m,k)) -> [cellRendererAccelAccelKey := fromIntegral k
                                                                               ,cellRendererAccelAccelMods := m
                                                                               ]

  get (keyTreeView . widgets) `on` keyPressEvent $ do
    k <- eventKeyVal
    m <- eventModifier
    liftIO $ do
      treeViewGetSelection (get (keyTreeView . widgets)) >>= treeSelectionGetSelected >>= \case
        Nothing -> return ()
        Just iter -> do
          (a,b) <- listStoreGetValue ls (listStoreIterToIndex iter)
          listStoreSetValue ls (listStoreIterToIndex iter) (a, (m,k))
    return True

  return ls

settingsSetCallback :: (?env :: Env) => ListStore (String, ([Modifier], KeyVal)) -> IO (Conf -> Conf) -> IO ()
settingsSetCallback ls widgetsToConf = do
  c <- readTVarIO (get conf)
  c' <- ($ c) <$> widgetsToConf

  l <- listStoreToList ls
  let c'' = foldl' (\c ((s,_,_),(_,mk)) -> setSetting c s mk)
              c'
              $ zip (toList keys) l

  let f c = (getSetting' c username, getSetting' c password)
      (u, p) = f c''
  when (f c /= (u, p)) $ setUsernameAndPassword (fromMaybe "" u) (fromMaybe "" p)
  
  get setConf c''

----------------------------------------------------------------

#ifdef LOCAL
dataFileName = return
#else
dataFileName = getDataFileName
#endif

loadConfig = do
  conf <- (newTVarIO =<<) $ try (readSettings settingsPlace) >>= \case
    Right (c, _) -> return c
    Left (_ :: IOException) -> return Map.empty

  (confE, confFire) <- newAddHandler
  let setConf c = do
        atomically $ writeTVar conf c
        confFire c
        saveSettings emptyDefaultConfig settingsPlace c
  return (conf, setConf, confE)

loadImages = do
  let imageFiles =
        [(TwoD, ["2D/" ++ (t : c : ".png") | c <- "gs", t <- "rcdhme"])
        ,(ThreeD, map ("3D/" ++)
                      ["GoldRabbit.png"
                      ,"GoldCat.png"
                      ,"GoldDog.png"
                      ,"GoldHorse.png"
                      ,"GoldCamel.png"
                      ,"GoldElephant.png"
                      ,"SilverRabbit.png"
                      ,"SilverCat.png"
                      ,"SilverDog.png"
                      ,"SilverHorse.png"
                      ,"SilverCamel.png"
                      ,"SilverElephant.png"
                      ])
        ]

  Map.fromList
    <$> mapM (_2 (fmap (listArray ((Gold, 0), (Silver, length pieceInfo - 1)))
                    . mapM ((>>= imageSurfaceCreateFromPNG) . dataFileName . ("images/" ++))))
             imageFiles

setupWidgets Widgets{setupGrid} = do
  setupIcons <- replicateM (length pieceInfo) drawingAreaNew
  setupLabels <- replicateM (length pieceInfo) $ labelNew (Nothing :: Maybe String)

  let x = div 140 6  -- TODO: get size automatically
    in forM_ setupIcons $ flip Gtk.set [widgetWidthRequest := x, widgetHeightRequest := x]

  zipWithM_ (\i n -> gridAttach setupGrid i n 0 1 1) setupIcons [0..]
  zipWithM_ (\l n -> gridAttach setupGrid l n 1 1 1) setupLabels [0..]
  
  return (setupIcons, setupLabels)

settingWidgets Widgets{settingsGrid, colourGrid} conf = do
  generalAccessor <- do
    Gtk.set settingsGrid [containerBorderWidth := 5]
    gridSetColumnSpacing settingsGrid 10
    gridSetRowSpacing settingsGrid 5

    (y, acc1) <- foldConfWidgets settingsGrid 0 0 generalSettings
    sep <- hSeparatorNew
    gridAttach settingsGrid sep 0 y 2 1
    (_, acc2) <- foldConfWidgets settingsGrid 0 (y+1) sharpSettings

    widgetShowAll settingsGrid
    return (acc1 <> acc2)

  colourAccessor <- do
    Gtk.set colourGrid [containerBorderWidth := 5]
    gridSetColumnSpacing colourGrid 10
    gridSetRowSpacing colourGrid 5

    (y, acc) <- foldConfWidgets colourGrid 0 0 colourSettings

    defaultButton <- buttonNewWithLabel "Defaults"
    gridAttach colourGrid defaultButton 0 y 1 1

    defaultButton `on` buttonActivated $ do
      atomically $ modifyTVar' conf (Map.union defaultColours)
      readTVarIO conf >>= snd acc

    widgetShowAll colourGrid
    return acc

  return $ generalAccessor <> colourAccessor

blind Widgets{blindModeMenu} = do
  first <- radioMenuItemNewWithLabel "Sighted"
  rest <- mapM (radioMenuItemNewWithLabelFromWidget first) ["Blind", "Show friendly", "Show enemy"]
  let l = [(True, True), (False, False), (True, False), (False, True)]
  (ah, fire) <- newAddHandler
  zipWithM_ (\item state -> do
                containerAdd blindModeMenu item
                item `on` checkMenuItemToggled $ do
                  b <- checkMenuItemGetActive item
                  when b $ fire state
            )
            (first : rest) l
  return ah

boardCoordinates' :: WidgetClass w => w -> (Double, Double) -> IO (Square, (Double, Double))
boardCoordinates' widget (x,y) = do
  s <- squareSize widget
  let [(u,a), (v,b)] = map (properFraction . (/ s) . subtract borderWidth) [x,y]
  return ((u,v), (a,b))

boardCoordinates :: WidgetClass w => w -> (Double, Double) -> IO Square
boardCoordinates widget (x,y) = fst <$> boardCoordinates' widget (x,y)

getEvents Widgets{boardCanvas, treeCanvas, flipBoard} Keys{..} setupIcons blindMode confE = do
  let setupIconsE = map (\icon -> icon `onE` buttonPressEvent $ return (Just ())) setupIcons

  (tick, tickFire) <- newAddHandler
  timeoutAdd (True <$ withHandler (tickFire ())) (div 1000 tickFrequency)

  (leftPress, leftPressFire) <- newAddHandler
  (rightPress, rightPressFire) <- newAddHandler

  boardCanvas `on` buttonPressEvent $ do
    b <- eventButton
    c <- eventCoordinates
    liftIO $ do
      (sq, x) <- boardCoordinates' boardCanvas c
      if inRange boardRange sq
        then do
          case b of
            LeftButton -> leftPressFire (sq, x)
            RightButton -> rightPressFire sq
          return True
        else return False

  let
    release = boardCanvas `onE` buttonReleaseEvent $ do
      b <- eventButton
      c <- eventCoordinates
      if b == LeftButton
        then liftIO $ Just <$> boardCoordinates boardCanvas c
        else return Nothing

    motion = boardCanvas `onE` motionNotifyEvent $ do
      c <- eventCoordinates
      sq <- liftIO $ boardCoordinates boardCanvas c
      if inRange boardRange sq
        then return (Just sq)
        else return Nothing

    treePress = treeCanvas `onE` buttonPressEvent $ Just <$> eventCoordinates

    flipE = flipBoard `onS` menuItemActivated

  (newGameE, newGameFire) <- newAddHandler
  
  return (Events{..}, newGameFire)

fullscreenKey Widgets{..} toggleFullscreenE = do
  fullscreen <- newIORef False

  void $ register toggleFullscreenE $ const $ readIORef fullscreen >>= \case
    False -> do
      widgetHide menuBar
      widgetHide statusLabel
      widgetHide buttonGrid
      widgetHide gameGrid
      windowFullscreen window
      writeIORef fullscreen True
    True -> do
      widgetShow menuBar
      widgetShow statusLabel
      widgetShow buttonGrid
      widgetShow gameGrid
      windowUnfullscreen window
      writeIORef fullscreen False

-- because no impredicative polymorphism
data UseEnv = UseEnv {getUseEnv :: (?env :: Env) => IO ()}

drawFuncs Widgets{boardCanvas, captureCanvas, treeCanvas} setupIcons conf = do
  let makeDraw :: WidgetClass w => w -> IO ((w -> Render ()) -> IO (), UseEnv)
      makeDraw w = do
        drawRef <- newIORef $ return ()
        let set r = do
              writeIORef drawRef (r w)
              widgetQueueDraw w
        return (set, UseEnv (void (w `on` draw $ join (liftIO (readIORef drawRef)))))

  (setDrawBoard, f1) <- makeDraw boardCanvas
  (setDrawSetupIcons, fl) <- unzip <$> mapM makeDraw setupIcons
  (setDrawCapture, f2) <- makeDraw captureCanvas
  (setDrawTree, f3) <- makeDraw treeCanvas

  return ((setDrawBoard, setDrawSetupIcons, setDrawCapture, setDrawTree)
         ,UseEnv (getUseEnv f1 >> mapM_ getUseEnv fl >> getUseEnv f2 >> getUseEnv f3))

extraHandlers :: (?env :: Env) => IO ()
extraHandlers = do
  let Widgets{..} = get widgets
  
  window `on` deleteEvent $ do
    liftIO $ do
      mainQuit
      readTVarIO (get gameroomRef) >>= \case
        Just g -> Protocol.logout g   -- TODO: timeout or something
        _ -> return ()
    return False

  myGamesItem `on` menuItemActivated
    $ background (timeout (5*10^6) (readMVar (get myGames)))
    $ maybe (return ()) serverGameCallback
  openGamesItem `on` menuItemActivated
    $ background (timeout (5*10^6) (readMVar (get openGames)))
    $ maybe (return ()) serverGameCallback
  watchGamesItem `on` menuItemActivated $ watchGameCallback
  recentGamesItem `on` menuItemActivated $ recentGamesCallback
  viewGameItem `on` menuItemActivated $ viewGameCallback
  playBotItem `on` menuItemActivated $ playBotCallback

  do
    b <- dialogAddButton settingsDialog "Apply" ResponseAccept
    boxReorderChild dialogActionArea b 0

  widgetGrabDefault =<< dialogAddButton settingsDialog "OK" ResponseOk

  (widgetsToConf, confToWidgets) <- settingWidgets (get widgets) (get conf)

  do
    ls <- initKeyList

    settingsItem `on` menuItemActivated $ do
      readTVarIO (get conf) >>= confToWidgets
      windowPresent settingsDialog

    settingsDialog `on` deleteEvent $ do
      liftIO $ widgetHide settingsDialog
      return True

    settingsDialog `on` response $ \case
      ResponseAccept -> settingsSetCallback ls widgetsToConf
      ResponseOk -> do
        settingsSetCallback ls widgetsToConf
        widgetHide settingsDialog
      _ -> return ()
  
  -- this is on realize so that the prompt-username window is centred over the main window
  void $ window `on` realize $ initialStuff

layoutHack Widgets{window, boardCanvas, topClock} = do
  sizeRef <- newIORef Nothing

  window `on` configureEvent $ do
    s <- eventSize
    liftIO $ readIORef sizeRef >>= \case
      Just s' | s' == s -> return ()
      _ -> do
        Gtk.set boardCanvas [widgetWidthRequest := 150
                            ,widgetHeightRequest := 150
                            ,widgetExpand := True
                            ]
        Gtk.set topClock [widgetHExpand := False]
        writeIORef sizeRef (Just s)

    return False

  boardCanvas `on` sizeAllocate $ \(Rectangle _ _ x y) -> postGUIAsync $ do
    let z = min x y
    Gtk.set boardCanvas [widgetWidthRequest := z
                        ,widgetHeightRequest := z
                        ,widgetExpand := False
                        ]
    Gtk.set topClock [widgetHExpand := True]

  return ()
  
----------------------------------------------------------------

main = do
  (conf, setConf, confE) <- loadConfig
  icons <- loadImages

  initGUI

  builder <- builderNew
  builderAddFromFile builder =<< dataFileName "nosteps.glade"
  widgets@Widgets{..} <- getWidgets builder

  (setupIcons, setupLabels) <- setupWidgets widgets

  ((setDrawBoard, setDrawSetupIcons, setDrawCapture, setDrawTree), drawHandlers) <-
    drawFuncs widgets setupIcons conf

  statusStack <- newTVarIO []

  myGames <- newEmptyMVar
  openGames <- newEmptyMVar
  liveGames <- newEmptyMVar
  postalGames <- newEmptyMVar
  recentGames <- newEmptyMVar
  botLadderBotsRef <- newTVarIO (return [])
  gameroomRef <- newTVarIO Nothing

  trapMask <- mkTrapMask 1.6 3.2 1.5

  let ?env = Env {..}

  getUseEnv drawHandlers
  
  setDrawBoard $ \canvas -> do
    x <- liftIO $ squareSize canvas
    translate borderWidth borderWidth
    scale x x

    liftIO (readTVarIO conf) >>= drawEmptyBoard False
  
  widgetAddEvents boardCanvas [ButtonPressMask, ButtonReleaseMask, Button1MotionMask]
  widgetAddEvents window [KeyPressMask]
  widgetAddEvents treeCanvas [ButtonPressMask]

  blindMode <- blind widgets

  keys@Keys{toggleFullscreenE, dummyGameE} <- initKeyActions widgets

  (events, newGameFire) <- getEvents widgets keys setupIcons blindMode confE

  fullscreenKey widgets toggleFullscreenE
  register dummyGameE $ \_ -> dummyGame (fromJust (parseTimeControl "15s/30s/100")) --"1d/30d/100/0/10m/0"))

  extraHandlers

  -- command line
  when True $ layoutHack widgets

  network <- compile (network events)
  writeIORef newGameRef $ \g -> do
    actuate network
    newGameFire g

  windowSetIconFromFile window =<< dataFileName "images/2D/cs.png"
  windowMaximize window
  widgetShowAll window

  mainGUI

  -- fails if exit is abnormal
  killSharps
