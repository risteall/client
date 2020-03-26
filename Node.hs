{-# LANGUAGE NamedFieldPuns, LambdaCase, DataKinds, GADTs, ScopedTypeVariables, KindSignatures, RankNTypes, TypeApplications, RecursiveDo #-}

module Node where

import Data.Unique
import System.Process
import System.Process.Internals
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Function as Function
import Data.List
import Text.Read hiding (get)
import Text.Regex
import Data.List.Split
import System.Directory
import System.IO
import Control.Concurrent
import Data.Maybe
import Data.Array
import Graphics.UI.Gtk(postGUIAsync)
import System.Posix.Signals
import Data.Traversable
import Data.Kind
import Control.Monad
import Text.Printf
import Data.IORef

import System.Mem.Weak

import Base
import Time
import GameTree
import Env
import Sharp

data Tag = Regular | Sharp | Dormant

--genSingletons [''Tag]

data RegularContent = RegularContent
  {move :: GenMove
  ,next :: Position
  ,times :: Maybe (Int, Int)
  }

killSharp :: SharpProcess -> IO ()
killSharp sp = do
  terminateProcess (sharpPH sp)
  signalPH (sharpPH sp) sigCONT
  modifyIORef' (get sharps) (filter (/= sp))

setStatus :: Bool -> SharpStatus -> SharpStatus
setStatus _ Stopped = Stopped
setStatus True _ = Running
setStatus False _ = Paused

-- toggle :: SharpStatus -> SharpStatus
-- toggle Running = Paused
-- toggle Paused = Running
-- toggle Stopped = Stopped

-- toggleSignal :: SharpStatus -> Maybe Signal
-- toggleSignal Running = Just sigSTOP
-- toggleSignal Paused = Just sigCONT
-- toggleSignal Stopped = Nothing

-- data SharpProcess = SharpProcess
--   {unique :: Unique
--   ,sharpPH :: ProcessHandle
--   ,status :: Behavior SharpStatus
--   ,val :: Behavior (Maybe SharpVal)
--   }


data Content :: Tag -> Type where
  CR :: RegularContent -> Content 'Regular
  CS :: SharpProcess -> Content 'Sharp
  CD :: RegularContent -> SharpProcess -> Bool -> Content 'Dormant

-- data family Content :: Tag -> Type
-- data instance Content 'Regular = CR RegularContent
-- data instance Content 'Sharp = CS SharpProcess
-- data instance Content 'Dormant = CD (RegularContent, SharpProcess)

data Node tag = Node
  {prev :: Position
  ,movelist :: [GenMove]  -- up to prev
  ,content :: Content tag
  }

class HasRegular tag where
  regular :: Content tag -> RegularContent

instance HasRegular Regular where
  regular (CR r) = r

instance HasRegular Dormant where
  regular (CD r _ _) = r

data SomeNode = forall tag. SomeNode (Node tag)

useRegular :: SomeNode -> (RegularContent -> a) -> Maybe a
useRegular (SomeNode n) f = case content n of
  CR r -> Just (f r)
  CD r _ _ -> Just (f r)
  CS _ -> Nothing

useRegular' :: Maybe SomeNode -> (forall r. HasRegular r => Maybe (Node r) -> a) -> Maybe a
useRegular' (Just (SomeNode n)) f = case content n of
  CR _ -> Just (f (Just n))
  CD _ _ _ -> Just (f (Just n))
  CS _ -> Nothing
useRegular' Nothing f = Just (f @'Regular Nothing)

instance Eq SomeNode where
  n1 == n2 = case useRegular n1 (\r1 -> useRegular n2 (\r2 -> next r1 == next r2)) of
    Just (Just b) -> b
    _ -> False

nextMovelist :: HasRegular r => Maybe (Node r) -> [GenMove]
nextMovelist = maybe [] (\n -> movelist n ++ [move (regular (content n))])

mkRegularNode :: HasRegular r => Maybe (Node r) -> GenMove -> Position -> Maybe (Int, Int) -> Node 'Regular
mkRegularNode node move position times = Node
  {prev = regularPosition node
  ,movelist = nextMovelist node
  ,content = CR RegularContent
    {move = move
    ,next = position
    ,times = times
    }
  }

mkSharpNode :: HasRegular r => [Move] -> Maybe (Node r) -> Maybe (Event () -> Event () -> Event () -> MomentIO (Node 'Sharp))
mkSharpNode excludes node = f <$> runSharp movelist' (regularPosition node) excludes
  where
    movelist' = nextMovelist node
    f :: ((SharpVal -> IO ()) -> IO () -> IO ProcessHandle) -> Event () -> Event () -> Event () -> MomentIO (Node 'Sharp)
    f run ePause eToggle eSecond = mdo
      (eVal, valFire) <- newEvent
      bVal <- stepper Nothing (Just <$> eVal)
      (eStopped, stoppedFire) <- newEvent
      ph <- liftIO $ run (postGUIAsync . valFire) (postGUIAsync (stoppedFire ()))
      let eSecond' = whenE ((== Running) <$> bStatus) eSecond
      bTimer <- accumB 0 ((+ 1) <$ eSecond')
      let eGo = whenE ((== Paused) <$> bStatus) eToggle
          eNoGo = foldr (unionWith const) never
                        [ePause
                        ,whenE ((== Running) <$> bStatus) eToggle
                        ,whenE ((== timeLimit) . Just <$> bTimer) eSecond'
                        ,() <$ filterE ((== (maybe "" show depthLimit)) . sharpDepth) eVal
                        ]
      bStatus <- accumB Running $ unions [setStatus True <$ eGo
                                         ,setStatus False <$ eNoGo
                                         ,const Stopped <$ eStopped
                                         ]
      reactimate $ do {signalPH ph sigCONT; putStrLn "Started"} <$ eGo
      reactimate $ do {signalPH ph sigSTOP; putStrLn "Paused"} <$ eNoGo
      u <- liftIO newUnique

      let sp = SharpProcess{unique = u
                           ,sharpPH = ph
                           ,status = bStatus
                           ,val = bVal
                           ,runtime = bTimer
                           }

      liftIO $ modifyIORef' (get sharps) (sp :)

      return Node{prev = regularPosition node
                 ,movelist = movelist'
                 ,content = CS sp
                 }

      -- return Node{prev = regularPosition node
      --            ,movelist = movelist'
      --            ,content = CS SharpProcess{unique = u
      --                                      ,status = bStatus
      --                                      ,val = bVal
      --                                      }
      --            }

-- mkSharpNode :: HasRegular r => Maybe (Node r) -> Event () -> Event () -> Maybe (MomentIO (Node 'Sharp, Behavior (Maybe SomeNode) -> Event a -> Event a))
-- mkSharpNode node eToggle eStop = f <$> runSharp movelist' (regularPosition node)
--   where
--     movelist' = nextMovelist node
--     f :: ((SharpVal -> IO ()) -> IO () -> IO ProcessHandle) -> MomentIO (Node 'Sharp, Behavior (Maybe SomeNode) -> Event a -> Event a)
--     f run = do
--       (bVal, valFire) <- newBehavior Nothing
--       (eStopped, stoppedFire) <- newEvent
--       ph <- liftIO $ run (postGUIAsync . valFire . Just) (postGUIAsync (stoppedFire ()))
--       bStatus <- accumB Running $ unions [toggle <$ eToggle
--                                          ,const Stopped <$ eStopped
--                                          ]
--       reactimate $ signalPH ph <$> filterJust (toggleSignal <$> (bStatus <@ eToggle))
--       reactimate $ terminateProcess ph <$ eStop
--       u <- liftIO newUnique
--       let filterEvents b = whenE ((== Just u) . getUnique <$> b)
--       return (Node{prev = regularPosition node
--                   ,movelist = movelist'
--                   ,content = CS SharpProcess{unique = u
--                                             ,status = bStatus
--                                             ,val = bVal
--                                             }
--                   }
--              ,filterEvents
--              )

-- doesn't pause Sharp
mkDormantNode :: Node Sharp -> Maybe (Int, Int) -> MomentIO (Maybe (Node 'Dormant))
mkDormantNode node times = fmap . f <$> valueB (status sharp) <*> valueB (val sharp)
  where
    CS sharp = content node
    f status SharpVal{sharpMove, sharpPosition} = Node
      {prev = prev node
      ,movelist = movelist node
      ,content = CD RegularContent{move = Right sharpMove
                                  ,next = sharpPosition
                                  ,times = times
                                  }
                    sharp
                    (status == Stopped)
      }

-- doesn't restart Sharp
detachDormantNode :: Node 'Dormant -> (Node 'Regular, Node 'Sharp)
detachDormantNode Node{prev, movelist, content = CD a b _}
  = (Node{prev, movelist, content = CR a}
    ,Node{prev, movelist, content = CS b}
    )

regularPosition :: HasRegular r => Maybe (Node r) -> Position
regularPosition = maybe newPosition (next . regular . content)

depth :: Maybe SomeNode -> Int
depth Nothing = 0
depth (Just (SomeNode n)) = 1 + posDepth (prev n)

toMove :: Maybe SomeNode -> Colour
toMove n | even (depth n) = Gold
         | otherwise = Silver

setupPhase :: Maybe SomeNode -> Bool
setupPhase = (< 2) . depth

----------------------------------------------------------------

getContent :: SomeNode -> Either RegularContent SharpProcess
getContent (SomeNode n) = case content n of
  CR r -> Left r
  CD r _ _ -> Left r
  CS s -> Right s

regularMove :: SomeNode -> Maybe GenMove
regularMove = either (Just . move) (const Nothing) . getContent

position :: Maybe SomeNode -> Behavior Position
position Nothing = pure newPosition
position (Just n@(SomeNode n')) = case getContent n of
  Left r -> pure (next r)
  Right s -> maybe (prev n') sharpPosition <$> val s

board :: Maybe SomeNode -> Behavior Board
board = fmap posBoard . position

getMove :: SomeNode -> Behavior (Maybe GenMove)
getMove n = case getContent n of
  Left r -> pure (Just (move r))
  Right s -> fmap (fmap (Right . sharpMove)) (val s)

getUnique :: Maybe SomeNode -> Maybe Unique
getUnique (Just (SomeNode n))
  | CS s <- content n
  = Just (unique s)
getUnique _ = Nothing

----------------------------------------------------------------


parseSharp :: Position -> String -> Maybe SharpVal
parseSharp p s
  | Just [d, e, pv] <- matchRegex r s
  , Just e' <- readEval e
  , (m:_) <- splitOn "  " pv
  , let move = parseMove (filter (not . (`elem` ["qpss", "pass"])) (words m))
  = Just SharpVal
    {sharpDepth = d
    ,sharpEval = e'
    ,sharpMove = move
    ,sharpPV = pv
    ,sharpPosition = either error id (playGenMove p (Right move))
    }
  | otherwise = Nothing
  where r = mkRegex "^ID Depth:[[:space:]]+([^[:space:]]+).*Eval:[[:space:]]+([^[:space:]]+).*PV:[[:space:]]+(.*)"

-- sharpLimit :: Either Int Int
-- sharpLimit = Right 60

-- -- time limit doesn't work with pausable Sharp
-- sharpLimit :: Int
-- sharpLimit = 16

timeLimit, depthLimit :: Maybe Int
timeLimit = Just 180
depthLimit = Nothing

sharpThreads :: Int
sharpThreads = 2

sharpExe = "/home/arimaa/sharp2015"

runSharp :: [GenMove] -> Position -> [Move] -> Maybe ((SharpVal -> IO ()) -> IO () -> IO ProcessHandle)
runSharp movelist position excludes = run <$> f
  where
    run (s, n) valCallback stoppedCallback = do
      tmpDir <- getTemporaryDirectory
      (tmp, h) <- openTempFile tmpDir ".movelist"
      hPutStr h s
      hClose h
      (_, Just hout, _, ph) <-
        createProcess (proc sharpExe (["analyze", tmp, n, "-threads", show sharpThreads]
                                      ++ if null excludes
                                           then []
                                           else ["-exclude", intercalate ", " (map show excludes)]))
          {std_out = CreatePipe}
--      hSetBuffering hout LineBuffering
      forkIO $ do
        s <- hGetContents hout
--        putStr s
        for (lines s) $ \l -> do
          putStrLn l
          maybe (return ()) valCallback $ parseSharp position l
--        mapM_ valCallback $ mapMaybe (parseSharp p) (lines s)
--        mapMaybe (parseSharp p) . lines <$> hGetContents hout
 --         >>= mapM_ valCallback
        waitForProcess ph
        removeFile tmp
        putStrLn "Stopped"
        stoppedCallback

      return ph

    nMoves = length movelist
    f | nMoves < 2 = Nothing
      | otherwise = Just (unlines (zipWith (\move n -> moveNum n ++ " " ++ showGenMove move) movelist [0..])
                         ,moveNum nMoves
                         )

signalPH :: ProcessHandle -> Signal -> IO ()
signalPH (ProcessHandle m _) s = readMVar m >>= \case
  OpenHandle pid -> signalProcess s pid
  _ -> return ()

movelistEntry :: SomeNode -> Behavior String
movelistEntry (SomeNode n) = case content n of
    CR r -> pure $ maybe "" showTreeDuration (fst <$> times r)
    CD _ s _ -> f s
    CS s -> f s
  where
    f s = maybe "" g <$> val s
    g v = printf "%s (d%s)"
                 (show $ (case posToMove (prev n) of Gold -> id; Silver -> flipEval) (sharpEval v))
                 (sharpDepth v)

nodeColour :: SomeNode -> Behavior (Maybe (Double, Double, Double))
nodeColour (SomeNode n) = case content n of
    CR _ -> pure Nothing
    CD _ s _ -> f s
    CS s -> f s
  where
    f s = Just . g <$> status s
    g Running = (0.2, 0.9, 0.2)
    g Paused = (0.3, 0.5, 0.8)
    g Stopped = (0.9, 0.2, 0.5)

----------------------------------------------------------------

-- invariants: Sharp nodes are always leaves; current node is not a Sharp node

-- returns: Nothing if sleep is impossible;
--       or Just (gt, s) where s might be a Sharp that should be paused
sleepAtView :: GameTree SomeNode -> MomentIO (Maybe (GameTree SomeNode, Maybe SharpProcess))
sleepAtView gt
  | Just (SomeNode n) <- viewNode gt
  , CS s <- content n
  = let
      f :: Node 'Dormant -> (GameTree SomeNode, Maybe SharpProcess)
      f d = (replaceView (SomeNode d) gt, Just s)
    in fmap (fmap f) $ mkDormantNode n Nothing
  | otherwise = return (Just (gt, Nothing))

-- sleepAtView :: GameTree SomeNode -> MomentIO (Maybe (GameTree SomeNode))
-- sleepAtView gt
--   | Just (SomeNode n) <- viewNode gt
--   , CS _ <- content n
--   = fmap (fmap f) $ mkDormantNode n Nothing
--   | otherwise = return (Just gt)
--   where
--     f :: Node 'Dormant -> GameTree SomeNode
--     f n' = replaceView (SomeNode n') gt

addFromRegular :: (forall r. HasRegular r => Maybe (Node r) -> Maybe (MomentIO SomeNode)) -> GameTree SomeNode -> MomentIO (GameTree SomeNode, Maybe SharpProcess)
addFromRegular f gt = sleepAtView gt >>= \case
  Nothing -> return (gt, Nothing)
  Just (gt', s) -> case join (useRegular' (viewNode gt') f) of
    Nothing -> return (gt, Nothing)
    Just x -> (\n -> (treePlan n gt', s)) <$> x

-- addFromRegular :: (forall r. HasRegular r => Maybe (Node r) -> Maybe (MomentIO SomeNode)) -> GameTree SomeNode -> MomentIO (GameTree SomeNode)
-- addFromRegular f gt = sleepAtView gt >>= \case
--   Nothing -> return gt
--   Just gt' -> case join (useRegular' (viewNode gt') f) of
--     Nothing -> return gt
--     Just x -> (\n -> treePlan n gt') <$> x

addSharp :: Event [SharpProcess] -> Event [SharpProcess] -> Event () -> GameTree SomeNode -> MomentIO (GameTree SomeNode, Maybe SharpProcess)
addSharp ePause eToggle eSecond gt = sleepAtView gt >>= \case
    Nothing -> return (gt, Nothing)
    Just (gt', s) -> do
      excludes' <- excludes
      case join (useRegular' (viewNode gt') (mkSharpNode excludes')) of
        Nothing -> return (gt, Nothing)
        Just f -> mdo
          ns <- f ePause' eToggle' eSecond
          let
            CS s' = content ns
            [ePause', eToggle'] = map ((() <$) . filterE (elem s')) [ePause, eToggle]
          return (treePlan (SomeNode ns) gt', s)
  where
    f :: SomeNode -> MomentIO (Maybe Move)
    f (SomeNode n) = case content n of
      CR _ -> return Nothing
      CS s -> fmap (fmap sharpMove) $ valueB (val s)
      CD _ s _ -> fmap (fmap sharpMove) $ valueB (val s)
    excludes = catMaybes <$> mapM f (belowView gt)

toggleSharp :: GameTree SomeNode -> (GameTree SomeNode, Maybe SharpProcess)
toggleSharp gt = case viewNode gt of
  Nothing -> (gt, Nothing)
  Just (SomeNode n) -> case content n of
    CR _ -> (gt, Nothing)
    CS s -> (gt, Just s)
    CD _ _ True -> (gt, Nothing)
    CD _ s False ->
        (if viewCanSharp gt
          then replaceView (SomeNode ns) gt
          else treePlan (SomeNode ns) $ goUp $ replaceView (SomeNode nr) gt
        ,Just s
        )
      where
        (nr, ns) = detachDormantNode n
        goUp gt' = select (init (viewPos gt')) gt'
