-- -*- Haskell -*-

{-# LANGUAGE LambdaCase, TupleSections, NamedFieldPuns, RecordWildCards, TypeApplications #-}

module Scrape where

import Data.Array.IArray
import Data.Maybe
import Data.List.Split
import Data.Char
import Data.List

import Network.HTTP hiding (Request, password)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.Regex
import Text.Read hiding (lift, get)

import Base

data ServerGameInfo = ServerGameInfo
  {sgiNames :: Array Colour String
  ,sgiRatings :: Array Colour Int
  ,sgiTimeControl :: TimeControl
  ,sgiRated :: Bool
  ,sgiResult :: (Colour, Reason)
  ,sgiMoves :: [(GenMove, Int)]
  }

getServerGame :: Int -> IO (Maybe ServerGameInfo)
getServerGame n = do
  s <- getResponseBody =<< (simpleHTTP $ getRequest $ "http://arimaa.com/arimaa/gameroom/opengamewin.cgi?gameid=" ++ show n)
  let f s = case matchRegex (mkRegex "arimaa\\.vars\\.(.*)=\"(.*)\"") s of
        Just [a, b] -> Just (a, b)
        _ -> Nothing
      assocs = mapMaybe f $ lines s
  return $ do
    [wplayer, bplayer, wrating, brating, timecontrol, rated, result, reason, movelist, timeused]
      <- mapM (flip lookup assocs)
              ["wplayer", "bplayer", "wrating", "brating", "timecontrol", "rated", "result", "reason", "movelist", "timeused"]
    rs <- mapM readMaybe [wrating, brating]
    tc <- parseTimeControl timecontrol
    r <- case reason of [c] -> readReason c; _ -> Nothing
    let ms = mapMaybe readGenMove $ splitOn "\\n" movelist
--    ms <- either (const Nothing) Just $ P.parse movesParser "" movelist
    ts <- mapM readMaybe $ splitOn " " timeused
    Just ServerGameInfo{sgiNames = colourArray [wplayer, bplayer]
                       ,sgiRatings = colourArray rs
                       ,sgiTimeControl = tc
                       ,sgiRated = rated == "1"
                       ,sgiResult = (if elem result ["w","g"] then Gold else Silver, r)
                       ,sgiMoves = zip ms ts
                       }

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



-- rmTag :: String -> [TagTree String] -> [TagTree String]
-- rmTag s [] = []
-- rmTag s (TagBranch s' as l : ts) | s == s' = rmTag s ts
--                                  | otherwise = TagBranch s' as (rmTag s l) : rmTag s ts
-- rmTag s (tl : ts) = tl : rmTag s ts

-- data LiveGameInfo = LiveGameInfo
--   {liveNames :: Array Colour String
--   ,liveRatings :: Array Colour Int
--   ,liveTimeControl :: TimeControl
--   ,liveRated :: Bool
--   ,liveGid :: String
--   } deriving Show

-- getLiveGames :: String -> IO [LiveGameInfo]
-- getLiveGames url = do
--   s <- getResponseBody =<< simpleHTTP (getRequest url)
--   let table = head [l | TagBranch "table" _ l <- universeTree $ parseTree s]
--       g tr | any (\case {TagBranch "table" _ _ -> True; _ -> False}) (universeTree tr) = Right tr
--            | otherwise = Left $ head [s | TagLeaf (TagText s) <- universeTree tr, not (all isSpace s)]
--       trs = [r | Right r <- takeWhile (/= Left "Scheduled Games") $ map g $ drop 2 [l | TagBranch "tr" _ l <- table]]
--       f tr = LiveGameInfo{liveNames = colourArray [gn, sn]
--                          ,liveRatings = colourArray $ map parseRating [gr, sr]
--                          ,liveTimeControl
--                          ,liveRated
--                          ,liveGid
--                          }
--         where
--              -- <sup> tags contain move numbers on the postal games page
--           a = [s | TagLeaf (TagText s) <- universeTree (rmTag "sup" tr), not (all isSpace s)]
--           (r, [gn, gr, tc, sn, sr]) = partition (== "R") a
--           liveRated = not (null r)
--           Just liveTimeControl = parseTimeControl tc
--           parseRating = head . mapMaybe readMaybe . words
--           ([liveGid]:_) = mapMaybe (matchRegex (mkRegex "openGame.*\\('([[:digit:]]+)"))
--                        [fromJust (lookup "href" attrs) | TagBranch "a" attrs _ <- universeTree tr]
--   return $ map f trs

-- text t = [s | TagLeaf (TagText s) <- universeTree t]

-- data RecentGameInfo = RecentGameInfo
--   {rgiNames :: Array Colour String
--   ,rgiRatings :: Array Colour Int
--   ,rgiWinner :: Colour
--   ,rgiRated :: Bool
--   ,rgiTimeControl :: TimeControl
--   ,rgiReason :: Reason
--   ,rgiMoveCount :: Int
--   ,rgiGid :: Int
--   } deriving Show

-- getRecentGames :: IO [RecentGameInfo]
-- getRecentGames = do
--   s <- getResponseBody =<< simpleHTTP (getRequest "http://arimaa.com/arimaa/gameroom/recentgames.cgi")
--   let table = [l | TagBranch "table" _ l <- universeTree $ parseTree s] !! 1
--       trs = drop 2 [l | TagBranch "tr" _ l <- table]
--       f tr = RecentGameInfo {rgiNames = colourArray [gn, sn]
--                             ,rgiRatings = colourArray [gr, sr]
--                             ,rgiWinner = fst $ fromJust $ find snd [(Gold, gw), (Silver, sw)]
--                             ,rgiRated = not (null r)
--                             ,rgiTimeControl = fromJust (parseTimeControl (filter (not . isSpace) tc))
--                             ,rgiReason = fromJust $ readReason $ head $ (filter (not . isSpace) (head (text reason)))
--                             ,rgiMoveCount = read @Int (head (text moves))
--                             ,rgiGid = gid
--                             }
--         where
--           [gold, board, silver, _, reason, moves, _, _time, _, _comments]
--             = [l | TagBranch "td" _ l <- tr]
--           g c = (name, head (mapMaybe (readMaybe @Int) (words r)), not (all isSpace (w1 ++ w2)))
--             where [w1, name, w2, r, _] = text c
--           [(gn, gr, gw), (sn, sr, sw)] = map g [gold, silver]
--           (r, [tc]) = partition (== "R") $ filter (not . all isSpace) $ text board
--           gid = head $ mapMaybe k $ map snd $ concat [a | TagBranch "a" a _ <- universeTree board]
--           k s | "openGame" `isInfixOf` s = readMaybe $ filter isDigit s
--               | otherwise = Nothing
--   return $ map f trs



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

--recent = "http://arimaa.com/arimaa/gameroom/recentgames.cgi"
--postal = "http://arimaa.com/arimaa/gameroom/postalgames.cgi"
