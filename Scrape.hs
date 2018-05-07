-- -*- Haskell -*-

{-# LANGUAGE LambdaCase, TupleSections, NamedFieldPuns, RecordWildCards #-}

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

-- maybeFail x = do
--   Just a <- x
--   return a

-- parseMovestring :: String -> Maybe [GenMove]
-- parseMovestring = traverse parseGenMove . splitOn "\\n"

-- movesParser :: P.Parsec String () [GenMove]
-- movesParser = P.sepBy (some P.digit *> P.oneOf "wbgs" *> some P.space *> (P.try (Left <$> setup) <|> Right <$> move))
--                       (P.string "\\n")
--   where
--     piece = maybeFail $ charToPiece <$> P.letter
--     square = maybeFail $ (\c1 c2 -> stringToSquare [c1,c2]) <$> P.letter <*> P.digit
--     dir = P.choice [Just North <$ P.char 'n'
--                  ,Just East <$ P.char 'e'
--                  ,Just South <$ P.char 's'
--                  ,Just West <$ P.char 'w'
--                  ,Nothing <$ P.char 'x'
--                  ]
--     setup = P.sepBy1 (flip (,) <$> piece <*> square) (some P.space)
--     move = Move <$> P.sepBy1 ((,,) <$> piece <*> square <*> dir) (some P.space)

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
rmTag s [] = []
rmTag s (TagBranch s' as l : ts) | s == s' = rmTag s ts
                                 | otherwise = TagBranch s' as (rmTag s l) : rmTag s ts
rmTag s (tl : ts) = tl : rmTag s ts

data LiveGameInfo = LiveGameInfo
  {liveNames :: Array Colour String
  ,liveRatings :: Array Colour Int
  ,liveTimeControl :: TimeControl
  ,liveRated :: Bool
  ,liveGid :: String
  } deriving Show

getLiveGames :: String -> IO [LiveGameInfo]
getLiveGames url = do
  s <- getResponseBody =<< simpleHTTP (getRequest url)
  let table = head [l | TagBranch "table" _ l <- universeTree $ parseTree s]
      g tr | any (\case {TagBranch "table" _ _ -> True; _ -> False}) (universeTree tr) = Right tr
           | otherwise = Left $ head [s | TagLeaf (TagText s) <- universeTree tr, not (all isSpace s)]
      trs = [r | Right r <- takeWhile (/= Left "Scheduled Games") $ map g $ drop 2 [l | TagBranch "tr" _ l <- table]]
      f tr = LiveGameInfo{liveNames = colourArray [gn, sn]
                         ,liveRatings = colourArray $ map parseRating [gr, sr]
                         ,liveTimeControl
                         ,liveRated
                         ,liveGid
                         }
        where
             -- <sup> tags contain move numbers on the postal games page
          a = [s | TagLeaf (TagText s) <- universeTree (rmTag "sup" tr), not (all isSpace s)]
          (r, [gn, gr, tc, sn, sr]) = partition (== "R") a
          liveRated = not (null r)
          Just liveTimeControl = parseTimeControl tc
          parseRating = head . mapMaybe readMaybe . words
          ([liveGid]:_) = mapMaybe (matchRegex (mkRegex "openGame.*\\('([[:digit:]]+)"))
                       [fromJust (lookup "href" attrs) | TagBranch "a" attrs _ <- universeTree tr]
  return $ map f trs
