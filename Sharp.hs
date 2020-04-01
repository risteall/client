{-# LANGUAGE LambdaCase, RecursiveDo, NamedFieldPuns, TupleSections, RecordWildCards #-}

module Sharp(SharpVal(..), Eval, flipEval, SharpStatus(..), SharpProcess(status, val), mkSharpProcess, killSharp, killSharps) where

import Data.Unique
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Process
import Text.Printf
import Data.Maybe
import Data.List
import Text.Read
import Data.IORef
import Text.Regex
import Data.List.Split
import System.Directory
import System.IO
import Control.Concurrent
import Data.Traversable
import Data.Time.Clock
import System.Process
import System.Process.Internals
import System.Posix.Signals
import System.IO.Unsafe
import Control.Monad
import Graphics.UI.Gtk(postGUIAsync)

import Base
import Env
import Settings
import WidgetValue

data SharpVal = SharpVal
  {sharpDepth :: String
  ,sharpEval :: Eval
  ,sharpMove :: Move
  ,sharpPV :: String
  ,sharpPosition :: Position
  }

data Eval = Millirabbits Int | Win Int | Loss Int

flipEval :: Eval -> Eval
flipEval (Millirabbits n) = Millirabbits (-n)
flipEval (Win n) = Loss n
flipEval (Loss n) = Win n

instance Show Eval where
  show (Millirabbits n) = printf "%.2f" (fromIntegral n / 1000 :: Double)
  show (Win n) = printf "#%d" n
  show (Loss n) = printf "#%d" (- n)

readEval :: String -> Maybe Eval
readEval s
  | Just n <- stripPrefix "Win" s
  , Just n' <- readMaybe n
  = Just $ Win n'
  | Just n <- stripPrefix "Loss" s
  , Just n' <- readMaybe n
  = Just $ Loss n'
  | Just n <- readMaybe s
  = Just $ Millirabbits n
  | otherwise = Nothing

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

runSharp :: [GenMove] -> Position -> [Move] -> Maybe ((SharpVal -> IO ()) -> IO () -> IO ProcessHandle)
runSharp movelist position excludes = run <$> f <*> getConf sharpExe
  where
    run (s, n) sharp valCallback stoppedCallback = do
      tmpDir <- getTemporaryDirectory
      (tmp, h) <- openTempFile tmpDir ".movelist"
      hPutStr h s
      hClose h
      (_, Just hout, _, ph) <-
        createProcess (proc sharp (["analyze", tmp, n, "-threads", show (getConf sharpThreads)]
                                   ++ if null excludes
                                        then []
                                        else ["-exclude", intercalate ", " (map show excludes)]))
          {std_out = CreatePipe}
      forkIO $ do
        s <- hGetContents hout
        for (lines s) $ \l -> do
          putStrLn l
          maybe (return ()) valCallback $ parseSharp position l
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

----------------------------------------------------------------

data SharpStatus = Running | Paused | Stopped deriving Eq

setStatus :: Bool -> SharpStatus -> SharpStatus
setStatus _ Stopped = Stopped
setStatus True _ = Running
setStatus False _ = Paused

data SharpProcess = SharpProcess
  {unique :: Unique
  ,ph :: ProcessHandle
  ,status :: Behavior SharpStatus
  ,val :: Behavior (Maybe SharpVal)
--  ,runtime :: Behavior Int
  ,used :: IORef UTCTime
  }

instance Eq SharpProcess where
  sp1 == sp2 = unique sp1 == unique sp2

----------------------------------------------------------------

signalPH :: ProcessHandle -> Signal -> IO ()
signalPH (ProcessHandle m _) s = readMVar m >>= \case
  OpenHandle pid -> signalProcess s pid
  _ -> return ()

sharps :: IORef [SharpProcess]
sharps = unsafePerformIO $ newIORef []

killSharp :: SharpProcess -> IO ()
killSharp SharpProcess{ph} = do
  terminateProcess ph
  signalPH ph sigCONT

registerSharp :: SharpProcess -> IO ()
registerSharp sp = do
  sharps' <- atomicModifyIORef' sharps (\ss -> (sp : ss, ss))
  when (length sharps' >= getConf maxSharps) $ do
    (s:_) <- map fst . sortOn snd <$> mapM (\s -> (s,) <$> readIORef (used s)) sharps'
    killSharp s

killSharps :: IO ()
killSharps = do
  readIORef sharps >>= mapM_ killSharp
  writeIORef sharps []

----------------------------------------------------------------

mkSharpProcess :: [GenMove] -> Position -> [Move] -> Maybe (Event () -> Event () -> Event () -> MomentIO SharpProcess)
mkSharpProcess movelist position excludes = f <$> runSharp movelist position excludes
  where
    f :: ((SharpVal -> IO ()) -> IO () -> IO ProcessHandle) -> Event () -> Event () -> Event () -> MomentIO SharpProcess
    f run ePause eToggle eSecond = mdo
      (eVal, valFire) <- newEvent
      val <- stepper Nothing (Just <$> eVal)
      (eStopped, stoppedFire) <- newEvent
      ph <- liftIO $ run (postGUIAsync . valFire) (postGUIAsync (stoppedFire ()))
      let eSecond' = whenE ((== Running) <$> status) eSecond
      bTimer <- accumB 0 ((+ 1) <$ eSecond')
      let eGo = whenE ((== Paused) <$> status) eToggle
          eNoGo = foldr (unionWith const) never
                        [ePause
                        ,whenE ((== Running) <$> status) eToggle
                        ,whenE ((== getConf sharpTimeLimit) . Just <$> bTimer) eSecond'
                        ,() <$ filterE ((== (maybe "" show (getConf sharpDepthLimit))) . sharpDepth) eVal
                        ]

      used <- liftIO $ getCurrentTime >>= newIORef
      reactimate $ (getCurrentTime >>= writeIORef used) <$ eGo

      status <- accumB Running $ unions [setStatus True <$ eGo
                                         ,setStatus False <$ eNoGo
                                         ,const Stopped <$ eStopped
                                         ]
      reactimate $ do {signalPH ph sigCONT; putStrLn "Started"} <$ eGo
      reactimate $ do {signalPH ph sigSTOP; putStrLn "Paused"} <$ eNoGo
      reactimate $ atomicModifyIORef' sharps (\ss -> (filter (/= sp) ss, ())) <$ eStopped

      unique <- liftIO newUnique

      let sp = SharpProcess{..}
      liftIO $ registerSharp sp
      return sp
