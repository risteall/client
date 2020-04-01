-- -*- Haskell -*-

{-# LANGUAGE LambdaCase, TupleSections, ScopedTypeVariables, NamedFieldPuns, MultiWayIf, PatternGuards, RecursiveDo, DeriveGeneric, DeriveAnyClass, RecordWildCards, StandaloneDeriving, CPP, DataKinds #-}

import Data.Array.IArray
import Graphics.UI.Gtk hiding (get, set, Shift, Arrow, rectangle)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo
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
import System.IO.Unsafe
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
import GHC.Exts
import Text.Printf
import Data.Unique
import Data.AppSettings hiding (saveSettings)
import System.Environment
import Control.Lens (_2)

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

#define LOCAL

#ifndef LOCAL
import Paths_nosteps
#endif

----------------------------------------------------------------

botLadderBots :: IO [BotLadderBot]
botLadderBots = join $ readTVarIO (get botLadderBotsRef)

gameroom :: IO Gameroom
gameroom = readTVarIO (get gameroomRef) >>= \case
  Just g -> return g
  Nothing -> do
    u <- getConf' username
    p <- getConf' password
    case (u, p) of
      (Just u'@(_:_), Just p'@(_:_)) -> Protocol.login u' p'
      _ -> error "Can't get gameroom"

----------------------------------------------------------------

setUsernameAndPassword :: String -> String -> IO ()
setUsernameAndPassword u p = do
  c <- readTVarIO (get conf)
  get setConf $ setSetting (setSetting c username (Just u)) password (Just p)
  atomically $ writeTVar (get gameroomRef) Nothing   -- bad: should logout
  getBotLadder

keyBindings :: [(Setting ([Modifier], KeyVal), String, Maybe (ButtonSet -> Button))]
keyBindings = map (\(a,b,c,d,e) -> (Setting a (b, keyFromName (fromString c)), d, e))
                  [("send-key", [], "s", "Send move", Just sendButton)
                  ,("resign-key", [], "r", "Resign", Just resignButton)
                  ,("sharp-key", [], "x", "Run Sharp", Just sharpButton)
                  ,("plan-key", [], "space", "Enter plan move", Just planButton)
                  ,("clear-key", [], "Escape", "Clear arrows", Nothing)
                  ,("prev-key", [], "Up", "Previous move", Just prevButton)
                  ,("next-key", [], "Down", "Next move", Just nextButton)
                  ,("start-key", [Gtk.Control], "Up", "Go to game start", Just startButton)
                  ,("end-key", [Gtk.Control], "Down", "Go to game end", Just endButton)
                  ,("current-key", [], "c", "Go to current game position", Just currentButton)
                  ,("prev-branch-key", [], "Left", "Previous variation", Nothing)
                  ,("next-branch-key", [], "Right", "Next variation", Nothing)
                  ,("delete-node-key", [], "BackSpace", "Remove plan move", Just deleteNodeButton)
                  ,("delete-line-key", [Gtk.Control], "BackSpace", "Remove plan variation (back to last branch)", Nothing)
                  ,("delete-all-key", [Gtk.Control, Gtk.Shift], "BackSpace", "Remove all plans", Just deleteAllButton)
                  ,("delete-from-here-key", [], "Delete", "Remove plans starting at current position", Nothing)
                  ,("toggle-sharp-key", [], "p", "Pause and unpause Sharp", Nothing)
                  ]

deriving instance Read Modifier

anyM :: Monad m => [m Bool] -> m Bool
anyM = foldr f (return False)
  where f x y = x >>= \case
          True -> return True
          False -> y

initKeyActions :: Window -> ButtonSet -> IO [AddHandler ()]
initKeyActions w bs = do
  l <- mapM (const newAddHandler) keyBindings
  forM_ (zip l keyBindings) $ \((_, fire), (_, _, mb)) -> forM_ mb $ \b -> b bs `on` buttonActivated $ fire ()
  w `on` keyPressEvent $ do
    k <- eventKeyVal
    m <- eventModifier
    let f (_, fire) (s, _, _) = do
          (m', k') <- getConf' s
          if k == k' && elem m (permutations m')
            then do {fire (); return True}
            else return False
    liftIO $ anyM $ zipWith f l keyBindings
  return $ map fst l

----------------------------------------------------------------
  
entryAccessor :: String -> IO (HBox, IO String, String -> IO ())
entryAccessor label = do
  box <- hBoxNew False 5
  l <- labelNew (Just label)
  e <- entryNew
  entrySetActivatesDefault e True
  containerAdd box l
  containerAdd box e

  return (box, entryGetText e, entrySetText e)

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

boardCoordinates' :: WidgetClass w => w -> (Double, Double) -> IO (Square, (Double, Double))
boardCoordinates' widget (x,y) = do
  s <- squareSize widget
  let [(u,a), (v,b)] = map (properFraction . (/ s) . subtract borderWidth) [x,y]
  return ((u,v), (a,b))

boardCoordinates :: WidgetClass w => w -> (Double, Double) -> IO Square
boardCoordinates widget (x,y) = fst <$> boardCoordinates' widget (x,y)

----------------------------------------------------------------

background :: IO a -> (a -> IO ()) -> IO ()
background x y = void $ forkIO $ x >>= postGUIAsync . y

showStatus :: IO ()
showStatus = do
  readTVarIO (get statusStack) >>= \case
    [] -> labelSetText (get (statusLabel . widgets)) ""
    (_,m):_ -> labelSetText (get (statusLabel . widgets)) m

setStatus :: String -> Maybe Int -> IO (IO ())
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

withStatus :: String -> IO a -> IO a
withStatus message action = do
  remove <- setStatus message Nothing
  finally action remove

sendMessage :: TChan (a, Int) -> TVar Int -> a -> IO Int
sendMessage ch r m = atomically $ do
  n <- readTVar r
  writeTChan ch (m, n)
  writeTVar r (n+1)
  return n

-- parseMoves :: Colour -> String -> (Colour, [(Colour, Message)])
-- parseMoves toMove moveString = (toMove', catMaybes l)
--   where
--     parseMoveNum s = case reads s :: [(Int,String)] of
--       [(_,"w")] -> Just Gold
--       [(_,"b")] -> Just Silver
--       _ -> Nothing
--     f :: Colour -> [String] -> (Colour, Maybe Message)
--     f _ (w:ws) | Just c <- parseMoveNum w
--                  = (c, g ws)
--     f c ws = (c, g ws)
--     g ws@(w:_) | length w == 3 = Just $ SendMove $ Left (parseSetup ws)
--                | length w == 4 = Just $ SendMove $ Right (parseMove ws)
--     g _ = Nothing
--     f' c move = case f c move of
--       (c', m) -> (c', (c',) <$> m)
--     (toMove', l) = mapAccumL f' toMove $ map words $ splitOn "\DC3" moveString

alert :: String -> IO ()
alert s = do
  x <- newEmptyMVar
  postGUIAsync $ do
    d <- messageDialogNew (Just (get (window . widgets))) [] MessageError ButtonsOk s
    widgetShowAll d
    d `on` response $ \_ -> do
      widgetDestroy d
      putMVar x ()
    return ()
  takeMVar x

toServer :: PlayInfo -> String -> TChan (Request, Int) -> TChan Int -> IO ()
toServer (gsurl, sid) auth requestChan responseChan = forever $ try f >>= \case
    Left (Protocol.ServerError e) -> alert e
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

fromServer :: PlayInfo -> [(String, String)] -> Bool -> TChan Update -> IO ()
fromServer (gsurl, sid) response started updateChan = do
  let [lc, ml, cl] = getFields ["lastchange", "moveslength", "chatlength"] response
  
  response' <- handle (\(Protocol.ServerError s) -> do {alert s; return response})
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

setServerGame :: GameInfo -> IO ()
setServerGame gameInfo = handle (\(Protocol.ServerError s) -> alert s) $ do
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
              (Just <$> channelEvent responseChan)
              (do
                  e <- channelEvent updateChan
                  b <- accumB newGameState (flip updateGameState <$> e)
                  return (b, e))
              (mapM_ killThread [t1, t2])

watchGame :: String -> IO ()
watchGame gid = handle (\(Protocol.ServerError s) -> alert s) $ do
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
    $ newGame GameParams{names = colourArray $ map (\s -> fromJust (lookup s response)) ["wplayer", "bplayer"]
                        ,ratings = colourArray $ map (\s -> lookup s response >>= readMaybe) ["wrating", "brating"]
                        ,isUser = mapColourArray $ const False
                        ,timeControl = tc
                        ,rated = fromJust (lookup "rated" response) == "1"
                        }
              [] (\_ -> return ()) (return Nothing)
              (do
                  e <- channelEvent updateChan
                  b <- accumB newGameState (flip updateGameState <$> e)
                  return (b, e))
              (killThread t1)

----------------------------------------------------------------

updateServerGames :: IO ()
updateServerGames = forever $ do
  gameroom <- gameroom
  Protocol.myGames gameroom >>= atomically . writeTVar (get myGames)
  Protocol.openGames gameroom >>= atomically . writeTVar (get openGames)
  getLiveGames "http://arimaa.com/arimaa/gameroom/watchgames.cgi" >>= atomically . writeTVar (get liveGames)
  getLiveGames "http://arimaa.com/arimaa/gameroom/postalgames.cgi" >>= atomically . writeTVar (get postalGames)
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

serverGameCallback :: [Protocol.GameInfo] -> IO ()
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

watchGameCallback :: IO ()
watchGameCallback = do
  liveGames <- readTVarIO (get liveGames)
  postalGames <- readTVarIO (get postalGames)
  d <- dialogNew
  Gtk.set d [windowTransientFor := get (window . widgets)
            ,windowDefaultWidth := 600
            ,windowDefaultHeight := 500
            ]

  dialogAddButton d "Cancel" ResponseCancel
  dialogAddButton d "Watch game" ResponseOk
  u <- castToContainer <$> dialogGetContentArea d

  (ts, tv) <- makeTreeStore [Node (Left "Live games") (map (\g -> Node (Right g) []) liveGames)
                            ,Node (Left "Postal games") (map (\g -> Node (Right g) []) postalGames)
                            ]
                            [("Gold", \x -> [cellText := either id (\lgi -> printf "%s (%d)"
                                                                                   (liveNames lgi ! Gold)
                                                                                   (liveRatings lgi ! Gold))
                                                                x])
                            ,("Silver", \x -> [cellText := either (const "") (\lgi -> printf "%s (%d)"
                                                                                             (liveNames lgi ! Silver)
                                                                                             (liveRatings lgi ! Silver))
                                                                  x])
                            ,("Time control", \x -> [cellText := either (const "") (show . liveTimeControl) x])
                            ,("Rated", \x -> [cellText := either (const "") (\lgi -> if liveRated lgi then "R" else "U") x])
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
          forkIO $ withStatus "Starting game" $ watchGame $ liveGid lgi
          widgetDestroy d
    _ -> widgetDestroy d

  return ()

----------------------------------------------------------------

makeDialog :: WidgetClass w => w -> String -> IO Bool -> IO ()
makeDialog w buttonText f = do
  d <- dialogNew
  Gtk.set d [windowTransientFor := get (window . widgets)]
  dialogAddButton d "Cancel" ResponseCancel
  widgetGrabDefault =<< dialogAddButton d buttonText ResponseOk
  u <- castToContainer <$> dialogGetContentArea d
  containerAdd u w
  widgetShowAll d
  d `on` response $ \case
    ResponseOk -> f >>= \case
      True -> widgetDestroy d
      False -> return ()
    _ -> widgetDestroy d
  return ()

viewGameCallback :: IO ()
viewGameCallback = do
  (w,g,_) <- entryAccessor "Enter game id:"
  makeDialog w "View game" $ g >>= \s -> case readMaybe s of
    Just n -> do
      viewGame n
      return True
    _ -> return False

expandGame :: TimeControl -> [(GenMove, Maybe Int)] -> Either String [Node.Node 'Node.Regular]
expandGame tc moves = snd <$> mapAccumLM f (Nothing, Just (initialReserve tc)) moves
  where
    f :: (Maybe (Node.Node 'Node.Regular), Maybe Int) -> (GenMove, Maybe Int) -> Either String ((Maybe (Node.Node 'Node.Regular), Maybe Int), Node.Node 'Node.Regular)
    f (n, r) (m, t) = (\p -> let r' = updateReserve tc <$> t <*> r
                                 n' = Node.mkRegularNode n m p ((,) <$> t <*> r')
                             in ((Just n', r'), n'))
                        <$> playGenMove (Node.regularPosition n) m

viewGame :: Int -> IO ()
viewGame n = do
  background (withStatus "Fetching game" (getServerGame n)) $ \(Just sgi) -> do  -- TODO: check return value
    let nodes = either error id $ expandGame (sgiTimeControl sgi) (map (second Just) (sgiMoves sgi))
        pos = Node.regularPosition $ if null nodes then Nothing else Just (last nodes)

    newGame GameParams{names = sgiNames sgi
                      ,ratings = Just <$> sgiRatings sgi
                      ,isUser = mapColourArray (const False)
                      ,timeControl = sgiTimeControl sgi
                      ,rated = sgiRated sgi
                      }
            (foldr (\n f -> [Node (Node.SomeNode n) f]) [] nodes)
            (\_ -> return ()) (return Nothing)
            (return (pure GameState{started = True, position = pos, result = Just $ sgiResult sgi},
                     never))
            (return ())

----------------------------------------------------------------

getBotLadder :: IO ()
getBotLadder = do
  u <- fromMaybe "" <$> getConf' username
  a <- async (botLadderAll (if null u then Nothing else Just u))
  atomically $ writeTVar (get botLadderBotsRef) $ wait a

startBot :: String -> Colour -> IO ()
startBot url c = do
  s <- getResponseBody =<< simpleHTTP (postRequestWithBody url "application/x-www-form-urlencoded"
                                                           (urlEncodeVars [("action", "player")
                                                                          ,("newgame", "Start Bot")
                                                                          ,("side", colourToServerChar c : [])
                                                                          ]))
  when (s /= "Bot started.\n") $ alert s

----------------------------------------------------------------

playBot :: BotLadderBot -> Colour -> IO ()
playBot bot c = void $ forkIO $ do
  withStatus "Starting bot" $ startBot (botUrl bot) (flipColour c)
  gameroom <- gameroom
  openGames <- withStatus "Contacting gameroom" $ Protocol.openGames gameroom
  case find (\gi -> Protocol.opponent gi == botName bot && Protocol.role gi == c) openGames of
    Nothing -> error "Missing game"   -- TODO: something better
    Just game -> withStatus "Starting game" $ setServerGame game

playBotCallback :: IO ()
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

  goldButton <- radioButtonNewWithLabel "Gold"
  silverButton <- radioButtonNewWithLabelFromWidget goldButton "Silver"
  mapM_ (containerAdd u) [goldButton, silverButton]

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
        Right bot -> fmap fst . find snd . zip [Gold, Silver] <$> mapM toggleButtonGetActive [goldButton, silverButton] >>= \case
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
  (ah, fire) <- newAddHandler
  newGame GameParams{names = mapColourArray (const "me")
                    ,ratings = mapColourArray (const Nothing)
                    ,isUser = mapColourArray (const True)
                    ,timeControl = tc
                    ,rated = False
                    }
          [] fire (return Nothing)
          (do
             e <- fromAddHandler ah
             let u = requestToUpdate <$> e
             b <- accumB newGameState (flip updateGameState <$> u)
             return (b, u)
          )
          (return ())

----------------------------------------------------------------

promptUsername :: IO () -> IO ()
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

initialStuff :: IO ()
initialStuff = do
  let x = do
        getBotLadder
        void $ forkIO updateServerGames
  u <- getConf' username
  p <- getConf' password
  case (u, p) of
    (Just _, Just _) -> x
    _ -> promptUsername x

----------------------------------------------------------------

initKeyList :: IO (ListStore (String, ([Modifier], KeyVal)))
initKeyList = do
  ls <- listStoreNew =<< mapM (\(setting, desc, _) -> (desc,) <$> getConf' setting) keyBindings
  treeViewSetModel (get (keyTreeView . widgets)) ls

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

settingsSetCallback :: ListStore (String, ([Modifier], KeyVal)) -> IO ()
settingsSetCallback ls = do
  c <- readTVarIO (get conf)
  c' <- ($ c) <$> get widgetsToConf

  l <- listStoreToList ls
  let c'' = foldl' (\c ((s,_,_),(_,mk)) -> setSetting c s mk)
                   c'
                   $ zip keyBindings l

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

main = do
  initialConf <- try (readSettings settingsPlace) >>= \case
    Right (c, _) -> return c
    Left (_ :: IOException) -> return Map.empty

  conf <- newTVarIO initialConf

  (confAH, confFire) <- newAddHandler
  let setConf c = do
        atomically $ writeTVar conf c
        saveSettings
        confFire c

  initGUI

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

  icons <- Map.fromList
    <$> mapM (_2 (fmap (listArray ((Gold, 0), (Silver, length pieceInfo - 1)))
                    . mapM ((>>= imageSurfaceCreateFromPNG) . dataFileName . ("images/" ++))))
             imageFiles
    
  builder <- builderNew
  builderAddFromFile builder =<< dataFileName "nosteps.glade"  -- TODO: don't hardcode filename
  buttonSet <- getButtonSet builder
  widgets@Widgets{..} <- getWidgets builder

  setupIcons <- replicateM (length pieceInfo) drawingAreaNew
  setupLabels <- replicateM (length pieceInfo) $ labelNew (Nothing :: Maybe String)

  let x = div 140 6  -- TODO: get size automatically
    in forM_ setupIcons $ flip Gtk.set [widgetWidthRequest := x, widgetHeightRequest := x]

  zipWithM_ (\i n -> gridAttach setupGrid i n 0 1 1) setupIcons [0..]
  zipWithM_ (\l n -> gridAttach setupGrid l n 1 1 1) setupLabels [0..]

----------------------------------------------------------------

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

  let (widgetsToConf, confToWidgets) = generalAccessor <> colourAccessor

----------------------------------------------------------------

  (getBlindMode, blindModeAH) <- do
    first <- radioMenuItemNewWithLabel "Sighted"
    rest <- mapM (radioMenuItemNewWithLabelFromWidget first) ["Blind", "Show friendly", "Show enemy"]
    let l = [(True, True), (False, False), (True, False), (False, True)]
        f = do
          x <- mapM checkMenuItemGetActive (first : rest)
          return $ fromMaybe (True, True) $ snd <$> find fst (zip x l)
    (ah, fire) <- newAddHandler
    zipWithM_ (\item state -> do
                  containerAdd blindModeMenu item
                  item `on` checkMenuItemToggled $ do
                    b <- checkMenuItemGetActive item
                    when b $ fire state
              )
              (first : rest) l
    return (f, ah)
  
  widgetAddEvents boardCanvas [ButtonPressMask, ButtonReleaseMask, Button1MotionMask]
  widgetAddEvents window [KeyPressMask]
  widgetAddEvents treeCanvas [ButtonPressMask]

  let layoutHack = True

  when layoutHack $ do
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

  let makeDraw :: WidgetClass w => w -> IO ((w -> Render ()) -> IO ())
      makeDraw w = do
        drawRef <- newIORef $ return ()
        w `on` draw $ join (liftIO (readIORef drawRef))
        return $ \r -> do
          writeIORef drawRef (r w)
          widgetQueueDraw w

  setDrawBoard <- makeDraw boardCanvas
  setDrawSetupIcons <- mapM makeDraw setupIcons
  setDrawCapture <- makeDraw captureCanvas
  setDrawTree <- makeDraw treeCanvas

  setDrawBoard $ \canvas -> do
    x <- liftIO $ squareSize canvas
    setSourceRGB 1 1 1
    paint
    translate borderWidth borderWidth
    scale x x

    drawEmptyBoard initialConf

  (leftPressAH, leftPressFire) <- newAddHandler
  (rightPressAH, rightPressFire) <- newAddHandler
  (motionAH, motionFire) <- newAddHandler
  (releaseAH, releaseFire) <- newAddHandler
  (flipAH, flipFire) <- newAddHandler

  setupIconAH <- forM setupIcons $ \icon -> do
    (ah, fire) <- newAddHandler
    icon `on` buttonPressEvent $ do {liftIO $ fire (); return True}
    return ah

  (tickAH, tickFire) <- newAddHandler
  timeoutAdd (True <$ tickFire ()) (div 1000 tickFrequency)

----------------------------------------------------------------

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

  boardCanvas `on` buttonReleaseEvent $ do
    b <- eventButton
    c <- eventCoordinates
    if b == LeftButton
      then liftIO $ do
        boardCoordinates boardCanvas c >>= releaseFire
        return True
      else return False

  boardCanvas `on` motionNotifyEvent $ do
    c <- eventCoordinates
    liftIO $ do
      sq <- boardCoordinates boardCanvas c
      if inRange boardRange sq
        then do
          motionFire sq
          return True
        else return False

  (treePressAH, treePressFire) <- newAddHandler

  treeCanvas `on` buttonPressEvent $ do
    c <- eventCoordinates
    liftIO $ treePressFire c
    return True

  flipBoard `on` menuItemActivated $ flipFire ()

  window `on` deleteEvent $ do
    liftIO $ do
      mainQuit
      readTVarIO (get gameroomRef) >>= \case
        Just g -> Protocol.logout g   -- TODO: timeout or something
        _ -> return ()
    return False

  myGamesItem `on` menuItemActivated $ readTVarIO (get myGames) >>= serverGameCallback
  openGamesItem `on` menuItemActivated $ readTVarIO (get openGames) >>= serverGameCallback
  watchGamesItem `on` menuItemActivated $ watchGameCallback
  viewGameItem `on` menuItemActivated $ viewGameCallback
  playBotItem `on` menuItemActivated $ playBotCallback

  widgetGrabDefault =<< dialogAddButton settingsDialog "OK" ResponseOk

----------------------------------------------------------------

  killGameRef <- newIORef (return ())
  statusStack <- newTVarIO []

  myGames <- newTVarIO []
  openGames <- newTVarIO []
  liveGames <- newTVarIO []
  postalGames <- newTVarIO []
  botLadderBotsRef <- newTVarIO (return [])
  gameroomRef <- newTVarIO Nothing

  [sendAH, resignAH, sharpAH, planAH, clearArrowsAH, prevAH, nextAH, startAH, endAH, currentAH, prevBranchAH, nextBranchAH
    ,deleteNodeAH, deleteLineAH, deleteAllAH, deleteFromHereAH, toggleSharpAH]
       <- initKeyActions window buttonSet

  trapMask <- mkTrapMask 1.6 3.2 1.5

  writeIORef globalEnv Env{..}

----------------------------------------------------------------

  do
    b <- dialogAddButton settingsDialog "Apply" ResponseAccept
    boxReorderChild dialogActionArea b 0

  do
    ls <- initKeyList

    settingsItem `on` menuItemActivated $ do
      readTVarIO conf >>= confToWidgets
      widgetShow settingsDialog

    settingsDialog `on` deleteEvent $ do
      liftIO $ widgetHide settingsDialog
      return True

    settingsDialog `on` response $ \case
      ResponseAccept -> settingsSetCallback ls
      ResponseOk -> do
        settingsSetCallback ls
        widgetHide settingsDialog
      _ -> return ()

  -- this is on realize so that the prompt-username window is centred over the main window
  window `on` realize $ initialStuff

  widgetShowAll window

  -- user plays self (for testing)
  args <- getArgs
  when (not (null args)) $ dummyGame (fromJust (parseTimeControl "1d/30d/100/0/10m/0"))

  mainGUI

  -- fails if exit is abnormal
  killSharps
