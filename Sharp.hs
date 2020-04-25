{-# LANGUAGE LambdaCase, RecursiveDo, NamedFieldPuns, TupleSections, RecordWildCards, ScopedTypeVariables, ImplicitParams, CPP #-}

module Sharp(SharpVal(..), Eval, flipEval, SharpStatus(..), SharpProcess(status, val), mkSharpProcess, killSharp, killSharps) where

import Data.Unique
import Reactive.Banana
import Reactive.Banana.Frameworks hiding (register)
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
import System.IO.Unsafe
import Graphics.UI.Gtk(postGUIAsync)
import Control.Exception
import Data.AppSettings

import Base
import Env
import Settings
import WidgetValue

#ifndef WindowsBuild
import System.Posix.Signals

-- !!!!!! shouldn't use internal; or at least figure out version dependency
signalPH :: ProcessHandle -> Signal -> IO ()
signalPH (ProcessHandle m _ _) s = readMVar m >>= \case
  OpenHandle pid -> signalProcess s pid
  _ -> return ()

pausePH, unpausePH :: ProcessHandle -> IO ()
pausePH ph = signalPH ph sigSTOP
unpausePH ph = signalPH ph sigCONT

#else
pausePH, unpausePH :: ProcessHandle -> IO ()
pausePH = terminateProcess
unpausePH _ = return ()
#endif

----------------------------------------------------------------

forkBracket :: IO a -> (a -> IO ()) -> (a -> IO c) -> IO a
forkBracket acquire release work = mask $ \restore -> do
  x <- acquire
  forkIO $ do
    restore (work x) `onException` release x
    release x
  return x

----------------------------------------------------------------

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

----------------------------------------------------------------

sharps :: MVar [(Unique, ProcessHandle, IORef UTCTime)]
sharps = unsafePerformIO $ newMVar []

register :: (?env :: Env) => ProcessHandle -> IO Unique
register ph = do
  u <- newUnique
  t <- getCurrentTime >>= newIORef
  l <- modifyMVar sharps (\ss -> return ((u, ph, t) : ss, ss))
  n <- getConf maxSharps
  mapM_ killPH . take (length l - n + 1) . map fst . sortOn snd
    =<< mapM (\(_, ph', t') -> (ph',) <$> readIORef t') l
  return u

unregister :: Unique -> IO ()
unregister u = modifyMVar_ sharps (\ss -> return (filter (\(u', _, _) -> u' /= u) ss))
  
useSharp :: Unique -> IO ()
useSharp u = readMVar sharps >>= f
  where
    f l = case mapMaybe (\(u', _, t) -> if u' == u then Just t else Nothing) l of
      t:_ -> getCurrentTime >>= writeIORef t
      _ -> return ()

killPH :: ProcessHandle -> IO ()
killPH ph = do
  terminateProcess ph
  unpausePH ph

killSharp :: SharpProcess -> IO ()
killSharp = killPH . ph

killSharps :: IO ()
killSharps = do
  takeMVar sharps >>= mapM_ (\(_, ph, _) -> killPH ph)
  putMVar sharps []

----------------------------------------------------------------

runSharp :: (?env :: Env) => [GenMove] -> Position -> [Move] -> (SharpVal -> IO ()) -> IO () -> IO (Maybe (ProcessHandle, Unique))
runSharp movelist position excludes valCallback stoppedCallback = do
    sharp <- getConf sharpExe
    catch (sequence $ run <$> f <*> sharp)
      (\(e :: IOException) -> do {print e; return Nothing})
  where
    nMoves = length movelist
    f | nMoves < 2 = Nothing
      | otherwise = Just (unlines (zipWith (\move n -> moveNum n ++ " " ++ showGenMove move)
                                     movelist [0..])
                         ,moveNum nMoves)

    run (s, n) sharp = do
      tmpDir <- getTemporaryDirectory

      bracketOnError
          (openTempFile tmpDir ".movelist")
          (\(tmp, h) -> do
            hClose h
            removeFile tmp
          ) $ \(tmp, h) -> do
        hPutStr h s
        hClose h
        nThreads <- getConf sharpThreads
        ((_, _, _, ph), u) <- forkBracket
            (do
               x@(_, _, _, ph) <-
                 createProcess (proc sharp (["analyze", tmp, n, "-threads", show nThreads]
                                            ++ if null excludes
                                                  then []
                                                  else ["-exclude", intercalate ", " (map show excludes)]))
                   {std_out = CreatePipe}
               u <- register ph
               return (x, u)
            )
            (\(x, u) -> do
               cleanupProcess x
               unregister u
               removeFile tmp
               putStrLn "Stopped"
               stoppedCallback
            ) $ \((_, Just hout, _, ph), _) -> do
          s <- hGetContents hout
          for (lines s) $ \l -> do
            putStrLn l
            maybe (return ()) valCallback $ parseSharp position l
          waitForProcess ph

        return (ph, u)

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
  }

instance Eq SharpProcess where
  sp1 == sp2 = unique sp1 == unique sp2

----------------------------------------------------------------

mkSharpProcess :: (?env :: Env) => [GenMove] -> Position -> [Move] -> Behavior Conf -> Event () -> Event () -> Event () -> MomentIO (Maybe SharpProcess)
mkSharpProcess movelist position excludes bConf ePause eToggle eSecond = do
  (eVal, valFire) <- newEvent
  val <- stepper Nothing (Just <$> eVal)
  (eStopped, stoppedFire) <- newEvent

  let
    f :: (ProcessHandle, Unique) -> MomentIO SharpProcess
    f (ph, unique) = mdo
      let eSecond' = whenE ((== Running) <$> status) eSecond
      bTimer <- accumB 0 ((+ 1) <$ eSecond')
      let
        g c v = case (getSetting' c sharpDepthLimit, readMaybe (sharpDepth v)) of
          (Just l, Just d) -> d >= l
          _ -> False
        eGo = whenE ((== Paused) <$> status) eToggle
        eNoGo = foldr (unionWith const) never
                  [ePause
                  ,whenE ((== Running) <$> status) eToggle
                  ,whenE ((\c t -> maybe False (<= t) (getSetting' c sharpTimeLimit)) <$> bConf <*> bTimer) eSecond'
                  ,() <$ filterApply (g <$> bConf) eVal
                  ]
      status <- accumB Running $ unions [setStatus True <$ eGo
                                        ,setStatus False <$ eNoGo
                                        ,const Stopped <$ eStopped
                                        ]
      reactimate $ do {unpausePH ph; useSharp unique; putStrLn "Started"} <$ eGo
      reactimate $ do {pausePH ph; putStrLn "Paused"} <$ eNoGo

      return SharpProcess{..}

  traverse f =<< liftIO (runSharp movelist position excludes (postGUIAsync . valFire) (postGUIAsync (stoppedFire ())))
