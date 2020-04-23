-- -*- Haskell -*-

{-# LANGUAGE LambdaCase, TupleSections, NamedFieldPuns, RecordWildCards, TypeApplications, DeriveGeneric #-}

module Scrape
  (ServerGameInfo(..), getServerGame
  ,BotLadderBot(..), botLadderAll
  ,ScrapeGameInfo(..), getGames
  ) where

import Data.Array.IArray
import Data.Maybe
import Data.List.Split
import Data.Char
import Data.List
import GHC.Generics
import Data.Aeson hiding (Array)
import Data.String
import Lens.Micro

import Network.HTTP hiding (Request, password)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.Regex
import Text.Read hiding (lift, get)

import Base

data GameJSON = GameJSON
  {gusername, susername :: String
  ,grating, srating :: String
  ,timecontrol :: String
  ,rated :: String
  ,result :: String
  ,termination :: String
--  ,movelist :: String
  ,events :: String
  } deriving Generic

instance FromJSON GameJSON

getGameJSON :: Int -> IO (Maybe GameJSON)
getGameJSON n = do
  s <- getResponseBody =<< (simpleHTTP $ getRequest $ "http://arimaa.com/arimaa/games/agn.cgi?gid=" ++ show n ++ "&f=json&c=gs")
  return $ decode (fromString s)

eventsLine :: String -> Maybe (Int, String)
eventsLine s = do
  [a, b] <- matchRegex (mkRegex "([[:digit:]]+)[[:space:]]*\\[[^]]*\\][[:space:]]*(.*)") s
  n <- readMaybe a
  return (n, b)

moveTimes :: [(Int, String)] -> Maybe [(GenMove, Int)]
moveTimes events
  | and $ zipWith (==) (map (fmap fst . snd) l) (Nothing : map (Just . moveNum) [0..])
  = Just $ zip moves times'
  | otherwise = Nothing
  where
    r1 = mkRegex "start.*received"
    r2 = mkRegex "move (.*) received.*\\[(.*)\\]"
    f s | isJust (matchRegex r1 s) = Just Nothing
        | Just [n, m] <- matchRegex r2 s
        , Just m' <- readGenMove m
        = Just (Just (n, m'))
        | otherwise = Nothing
    l = mapMaybe (_2 f) events
    moves = mapMaybe (fmap snd . snd) l
    times = map fst l
    times' = zipWith (-) (tail times) times

data ServerGameInfo = ServerGameInfo
  {sgiNames :: Array Colour String
  ,sgiRatings :: Array Colour Int
  ,sgiTimeControl :: TimeControl
  ,sgiRated :: Bool
  ,sgiResult :: (Colour, Reason)
  ,sgiMoves :: [(GenMove, Int)]
  } deriving Show

readJSON :: GameJSON -> Maybe ServerGameInfo
readJSON GameJSON{..} = do
  gr <- readMaybe grating
  sr <- readMaybe srating
  tc <- parseTimeControl timecontrol
  let rated' = rated == "1"
  result' <- case result of
    [c] -> charToColour c
    _ -> Nothing
  reason <- case termination of
    [c] -> readReason c
    _ -> Nothing
  mt <- moveTimes $ mapMaybe eventsLine (lines events)
  return ServerGameInfo
    {sgiNames = colourArray [gusername, susername]
    ,sgiRatings = colourArray [gr, sr]
    ,sgiTimeControl = tc
    ,sgiRated = rated'
    ,sgiResult = (result', reason)
    ,sgiMoves = mt
    }

getServerGame :: Int -> IO (Maybe ServerGameInfo)
getServerGame n = (>>= readJSON) <$> getGameJSON n
  
--   s <- getResponseBody =<< (simpleHTTP $ getRequest $ "http://arimaa.com/arimaa/gameroom/opengamewin.cgi?gameid=" ++ show n)
--   let f s = case matchRegex (mkRegex "arimaa\\.vars\\.(.*)=\"(.*)\"") s of
--         Just [a, b] -> Just (a, b)
--         _ -> Nothing
--       assocs = mapMaybe f $ lines s
--   return $ do
--     [wplayer, bplayer, wrating, brating, timecontrol, rated, result, reason, movelist, timeused]
--       <- mapM (flip lookup assocs)
--               ["wplayer", "bplayer", "wrating", "brating", "timecontrol", "rated", "result", "reason", "movelist", "timeused"]
--     rs <- mapM readMaybe [wrating, brating]
--     tc <- parseTimeControl timecontrol
--     r <- case reason of [c] -> readReason c; _ -> Nothing
--     let ms = mapMaybe readGenMove $ splitOn "\\n" movelist
-- --    ms <- either (const Nothing) Just $ P.parse movesParser "" movelist
--     ts <- mapM readMaybe $ splitOn " " timeused
--     Just ServerGameInfo{sgiNames = colourArray [wplayer, bplayer]
--                        ,sgiRatings = colourArray rs
--                        ,sgiTimeControl = tc
--                        ,sgiRated = rated == "1"
--                        ,sgiResult = (if elem result ["w","g"] then Gold else Silver, r)
--                        ,sgiMoves = zip ms ts
--                        }

----------------------------------------------------------------

data BotLadderBot = BotLadderBot
  {botName :: String
  ,botUrl :: String
  ,botRating, botRatingk :: Int
  ,nGames, nGold, nSilver, nWon, nLost :: Int
  } deriving Show

botLadderAll :: Maybe String -> IO [BotLadderBot]
botLadderAll name = do
  s <- getResponseBody =<< simpleHTTP (getRequest ("http://arimaa.com/arimaa/gameroom/botLadderAll.cgi" ++ maybe "" ("?u=" ++) name)) 
  let trs = tail [l | TagBranch "tr" _ l <- universeTree $ parseTree s]
      f tr = BotLadderBot{..}
        where
          tds = [m | TagBranch "td" _ m <- tr]
          [botName] = [s | TagBranch "a" _ [TagLeaf (TagText s)] <- tds !! 0]
          [botUrl] = [s | TagBranch "a" [("href", s)] _ <- tds !! 3]
          [botRating, botRatingk, nGames, nGold, nSilver, nWon, nLost]
            = map (read . (\case [TagLeaf (TagText x)] -> x; _ -> "0") . (tds !!)) [1,2,5,6,7,8,9]
  return $ map f trs

----------------------------------------------------------------

rmTag :: String -> [TagTree String] -> [TagTree String]
rmTag s = transformTree f
  where
    f (TagBranch s' _ _) | s' == s = []
    f x = [x]

data ScrapeGameInfo = ScrapeGameInfo
  {giNames :: Array Colour String
  ,giRatings :: Array Colour Int
  ,giWinner :: Maybe Colour
  ,giGid :: Int
  ,giTimeControl :: TimeControl
  ,giRated :: Bool
  ,giReason :: Maybe Reason
  ,giMoveCount :: Maybe Int
  } deriving Show

readPlayer :: TagTree String -> Maybe (String, Int, Bool)
readPlayer (TagBranch "td" _ l)
  | [n] <- mapMaybe name l'
  , [r] <- mapMaybe rating l'
  = Just (n, r, any winner l')
  where
    l' = universeTree $ rmTag "sup" l
    name (TagBranch "a" attrs sub)
      | Just href <- lookup "href" attrs
      , "playerPage" `isInfixOf` href
      , [s] <- [s | TagLeaf (TagText s) <- universeTree sub]
      = Just s
    name _ = Nothing
    rating (TagLeaf (TagText s)) = listToMaybe $ mapMaybe readMaybe (words s)
    rating _ = Nothing
    winner (TagLeaf (TagText s)) = filter (not . isSpace) s == "*"
    winner _ = False
readPlayer _ = Nothing

allEq :: Eq a => [a] -> Maybe a
allEq (x:xs) | all (== x) xs = Just x
allEq _ = Nothing

readBoard :: TagTree String -> Maybe (Int, TimeControl, Bool)
readBoard (TagBranch "td" _ l)
  | [tc] <- mapMaybe timeControl l'
  , Just gid' <- allEq (mapMaybe gid l')
  = Just (gid', tc, any rated l')
  where
    l' = universeTree l
    gid (TagBranch "a" attrs _)
      | Just href <- lookup "href" attrs
      , Just [_,s] <- matchRegex (mkRegex "(openGame|jsShowGame)[^[:digit:]]*([[:digit:]]+)") href
      = readMaybe s
    gid _ = Nothing
    timeControl (TagLeaf (TagText s)) = parseTimeControl (filter (not . isSpace) s)
    timeControl _ = Nothing
    rated (TagLeaf (TagText s)) = filter (not . isSpace) s == "R"
    rated _ = False
readBoard _ = Nothing

strings :: [TagTree String] -> [String]
strings t = [s | TagLeaf (TagText s) <- universeTree t]

reason :: TagTree String -> Maybe Reason
reason (TagBranch "td" _ l)
  | [s] <- strings l
  = f s
  where
    f s = case filter (not . isSpace) s of
      [c] -> readReason c
      _ -> Nothing
reason _ = Nothing

moveCount :: TagTree String -> Maybe Int
moveCount (TagBranch "td" _ l)
  | [s] <- strings l
  = readMaybe s
moveCount _ = Nothing

readTr :: [TagTree String] -> Maybe ScrapeGameInfo
readTr l
  | [(s1, r1, w1), (s2, r2, w2)] <- mapMaybe readPlayer l
  , [(gid, tc, rated)] <- mapMaybe readBoard l
  = Just ScrapeGameInfo{giNames = colourArray [s1, s2]
                       ,giRatings = colourArray [r1, r2]
                       ,giWinner = fmap snd $ find fst $ zip [w1, w2] [Gold, Silver]
                       ,giGid = gid
                       ,giTimeControl = tc
                       ,giRated = rated
                       ,giReason = r
                       ,giMoveCount = n
                       }
  where
    r = case mapMaybe reason l of
      [r] -> Just r
      _ -> Nothing
    n = case mapMaybe moveCount l of
      [n] -> Just n
      _ -> Nothing
readTr _ = Nothing

getGames url = do
  s <- getResponseBody =<< simpleHTTP (getRequest url)
  return $ mapMaybe readTr [l | TagBranch "tr" _ l <- universeTree (parseTree s)]
