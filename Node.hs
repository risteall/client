{-# LANGUAGE NamedFieldPuns, LambdaCase, DataKinds, GADTs, ScopedTypeVariables, KindSignatures, RankNTypes, TypeApplications, RecursiveDo, ImplicitParams #-}

module Node where

import Data.Unique
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.List
import Data.Maybe
import Data.Kind
import Control.Monad
import Text.Printf
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent
import Data.Time.Clock
import Data.Colour.SRGB (RGB)
import Data.AppSettings

import Base
import Misc
import GameTree
import Sharp
import Env
import Settings

data RegularContent = RegularContent
  {move :: GenMove
  ,next :: Position
  ,times :: Maybe (Int, Int)
  }

data Tag = Regular | Sharp | Dormant

--genSingletons [''Tag]

data Content :: Tag -> Type where
  CR :: RegularContent -> Content 'Regular
  CS :: SharpProcess -> Content 'Sharp
  CD :: RegularContent -> SharpProcess -> Content 'Dormant

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
  regular (CD r _) = r

regularPosition :: HasRegular r => Maybe (Node r) -> Position
regularPosition = maybe newPosition (next . regular . content)

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

mkSharpNode :: (?env :: Env, HasRegular r) => [Move] -> Maybe (Node r) -> Behavior Conf -> Event () -> Event () -> Event () -> MomentIO (Maybe (Node 'Sharp))
mkSharpNode excludes node bConf e1 e2 e3 =
    fmap (fmap f) $ mkSharpProcess m p excludes bConf e1 e2 e3
  where
    p = regularPosition node
    m = nextMovelist node
    f s = Node p m (CS s)

-- mkSharpNode :: HasRegular r => [Move] -> Maybe (Node r) -> Maybe (Event () -> Event () -> Event () -> MomentIO (Node 'Sharp))
-- mkSharpNode excludes node = f <$> mkSharpProcess m p excludes
--   where
--     p = regularPosition node
--     m = nextMovelist node
--     f g e1 e2 e3 = Node p m . CS <$> g e1 e2 e3

-- doesn't pause Sharp
mkDormantNode :: Node 'Sharp -> Maybe (Int, Int) -> MomentIO (Maybe (Node 'Dormant))
mkDormantNode node times = fmap (fmap f) $ valueB (val sharp)
  where
    CS sharp = content node
    f SharpVal{sharpMove, sharpPosition} = Node
      {prev = prev node
      ,movelist = movelist node
      ,content = CD RegularContent{move = Right sharpMove
                                  ,next = sharpPosition
                                  ,times = times
                                  }
                    sharp
      }

-- doesn't restart Sharp
detachDormantNode :: Node 'Dormant -> (Node 'Regular, Node 'Sharp)
detachDormantNode Node{prev, movelist, content = CD a b}
  = (Node{prev, movelist, content = CR a}
    ,Node{prev, movelist, content = CS b}
    )

----------------------------------------------------------------

data SomeNode = forall tag. SomeNode (Node tag)

useRegular :: SomeNode -> (RegularContent -> a) -> Maybe a
useRegular (SomeNode n) f = case content n of
  CR r -> Just (f r)
  CD r _ -> Just (f r)
  CS _ -> Nothing

-- bad: treeMove
instance Eq SomeNode where
  n1 == n2 = case useRegular n1 (\r1 -> useRegular n2 (\r2 -> next r1 == next r2)) of
    Just (Just b) -> b
    _ -> False

getContentR :: SomeNode -> Either RegularContent SharpProcess
getContentR (SomeNode n) = case content n of
  CR r -> Left r
  CD r _ -> Left r
  CS s -> Right s

getContentS :: SomeNode -> Either RegularContent SharpProcess
getContentS (SomeNode n) = case content n of
  CR r -> Left r
  CD _ s -> Right s
  CS s -> Right s

useRegular' :: Maybe SomeNode -> (forall r. HasRegular r => Maybe (Node r) -> a) -> Maybe a
useRegular' (Just (SomeNode n)) f = case content n of
  CR _ -> Just (f (Just n))
  CD _ _ -> Just (f (Just n))
  CS _ -> Nothing
useRegular' Nothing f = Just (f @'Regular Nothing)

----------------------------------------------------------------

depth :: Maybe SomeNode -> Int
depth Nothing = 0
depth (Just (SomeNode n)) = 1 + posDepth (prev n)

toMove :: Maybe SomeNode -> Colour
toMove n | even (depth n) = Gold
         | otherwise = Silver

setupPhase :: Maybe SomeNode -> Bool
setupPhase = (< 2) . depth

regularMove :: SomeNode -> Maybe GenMove
regularMove = either (Just . move) (const Nothing) . getContentR

----------------------------------------------------------------

position :: Maybe SomeNode -> Behavior Position
position Nothing = pure newPosition
position (Just n@(SomeNode n')) = case getContentR n of
  Left r -> pure (next r)
  Right s -> maybe (prev n') sharpPosition <$> val s

board :: Maybe SomeNode -> Behavior Board
board = fmap posBoard . position

getMove :: Maybe SomeNode -> Behavior (Maybe GenMove)
getMove Nothing = pure Nothing
getMove (Just n) = case getContentR n of
  Left r -> pure (Just (move r))
  Right s -> fmap (fmap (Right . sharpMove)) (val s)

movelistEntry :: SomeNode -> Behavior String
movelistEntry n@(SomeNode n') = case getContentS n of
    Left r -> pure $ maybe "" showTreeDuration (fst <$> times r)
    Right s -> maybe "" g <$> val s
  where
    g v = printf "%s (d%s)"
                 (show $ (case posToMove (prev n') of Gold -> id; Silver -> flipEval) (sharpEval v))
                 (sharpDepth v)

nodeColour :: (?env :: Env) => SomeNode -> Behavior (Maybe (IO (RGB Double)))
nodeColour n = case getContentS n of
    Left _ -> pure Nothing
    Right s -> Just . g <$> status s
  where
    g Running = getConf runningSharpColour
    g Paused = getConf pausedSharpColour
    g Stopped = getConf stoppedSharpColour

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

addFromRegular :: (forall r. HasRegular r => Maybe (Node r) -> Maybe (MomentIO SomeNode)) -> GameTree SomeNode -> MomentIO (GameTree SomeNode, Maybe SharpProcess)
addFromRegular f gt = sleepAtView gt >>= \case
  Nothing -> return (gt, Nothing)
  Just (gt', s) -> case join (useRegular' (viewNode gt') f) of
    Nothing -> return (gt, Nothing)
    Just x -> (\n -> (treePlan n gt', s)) <$> x

addSharp :: (?env :: Env) => Behavior Conf -> Event [SharpProcess] -> Event [SharpProcess] -> Event () -> GameTree SomeNode -> MomentIO (GameTree SomeNode, Maybe SharpProcess)
addSharp bConf ePause eToggle eSecond gt = sleepAtView gt >>= \case
    Nothing -> return (gt, Nothing)
    Just (gt', s) -> do
      excludes' <- excludes
      case useRegular' (viewNode gt') (mkSharpNode excludes') of
        Nothing -> return (gt, Nothing)
        Just f -> mdo
          mns <- f bConf ePause' eToggle' eSecond
          let [ePause', eToggle'] = case content <$> mns of
                Nothing -> [never, never]
                Just (CS s') -> map ((() <$) . filterE (elem s')) [ePause, eToggle]
          case mns of
            Nothing -> return (gt, Nothing)
            Just ns -> return (treePlan (SomeNode ns) gt', s)
  where
    f :: SomeNode -> MomentIO (Maybe Move)
    f n = case getContentS n of
      Left _ -> return Nothing
      Right s -> fmap (fmap sharpMove) $ valueB (val s)
    excludes = catMaybes <$> mapM f (belowView gt)

toggleSharp :: GameTree SomeNode -> MomentIO (GameTree SomeNode, Maybe SharpProcess)
toggleSharp gt = case viewNode gt of
  Nothing -> return (gt, Nothing)
  Just (SomeNode n) -> case content n of
    CR _ -> return (gt, Nothing)
    CS s -> return (gt, Just s)
    CD _ s -> f <$> valueB (status s)
      where
        f Stopped = (gt, Nothing)
        f _ = (if viewCanSharp gt
                then replaceView (SomeNode ns) gt
                else treePlan (SomeNode ns) $ goUp $ replaceView (SomeNode nr) gt
              ,Just s
              )
        (nr, ns) = detachDormantNode n
        goUp gt' = select (init (viewPos gt')) gt'

killNode :: SomeNode -> IO ()
killNode n = case Node.getContentS n of
  Left _ -> return ()
  Right s -> killSharp s
