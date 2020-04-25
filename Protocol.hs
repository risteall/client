module Protocol where

import Control.Exception
import Data.Typeable
import Network.HTTP
import Control.Concurrent
import System.IO.Error
import Data.Bifunctor
import Data.List.Split
import Data.Maybe

import Base

data ServerError = ServerError String deriving (Show, Typeable)

instance Exception ServerError

gameroomUrl = "http://arimaa.com/arimaa/gameroom/client1gr.cgi"

  -- TODO: timeout ?
arimaaPost :: String -> [(String, String)] -> IO [(String, String)]
arimaaPost url body = do
  let request = getResponseBody =<< (simpleHTTP $ postRequestWithBody url "application/x-www-form-urlencoded" (urlEncodeVars body))
      f n = catchIOError request $ \e -> do
        putStrLn $ "Error in sending" ++ maybe "" (' ':) (lookup "action" body) ++ " request (attempt " ++ show n ++ "): " ++ show e
        threadDelay 2000000
        f (n+1)
  s <- f 1
  let ret = map (second tail . span (/= '=')) $ filter (/= "--END--") $ lines $ urlDecode s
  case lookup "error" ret of
    Nothing -> return ret
    Just e -> throwIO $ ServerError e

data Gameroom = Gameroom {grid :: String, sid :: String} deriving Show

data GameInfo = GameInfo
  { gid :: String
  , role :: Colour
  , timecontrol :: TimeControl
  , rated :: Bool
  , postal :: Bool
  , createdts :: String
  , opponent :: String
  } deriving Show

data ReserveInfo = ReserveInfo
  { rgid :: String                -- is this the same as the other gid ?
  , gsurl :: String
  , tid :: String
  } deriving Show

type PlayInfo = (String, String) -- gsurl, sid
{-
data ServerGame = ServerGame
  { auth :: String
  , lastChange :: String
  , gameMoves :: [String]
  }
-}
getFields :: [String] -> [(String, String)] -> [String]
getFields fields x = fromMaybe (error "foo") $ sequenceA $ map (flip lookup x) fields

parseGameInfo :: String -> GameInfo
parseGameInfo s = f $ getFields ["gid", "role", "timecontrol", "rated", "postal", "createdts", "opponent"] x
  where
    x = map (\q -> case splitOn "=" q of [a,b] -> (a,b)) $ endBy "\DC3" $ urlDecode s
    f [gid, role, timecontrol, rated, postal, createdts, opponent]
      | Just tc <- parseTimeControl timecontrol
        = GameInfo gid
                   (if role=="w" then Gold else Silver)
                   tc
                   (rated == "1")
                   (postal == "1")
                   createdts
                   opponent
      | otherwise = error "foo"

parseGames :: [(String, String)] -> [GameInfo]
parseGames x = map (parseGameInfo . fromJust) $ takeWhile isJust $ map (\n -> lookup (show n) x) [1..]

login :: String -> String -> IO Gameroom
login username password = do
  a <- arimaaPost gameroomUrl [("action", "login"), ("username", username), ("password", password)]
  let [sid, grid] = getFields ["sid", "grid"] a
  return $ Gameroom grid sid

--createGame

myGames :: Gameroom -> IO [GameInfo]
myGames gameroom = fmap parseGames $ arimaaPost gameroomUrl [("action", "mygames"), ("sid", sid gameroom)]

--invitedMeGames

openGames :: Gameroom -> IO [GameInfo]
openGames gameroom = fmap parseGames $ arimaaPost gameroomUrl [("action", "opengames"), ("sid", sid gameroom)]

reserveSeat :: Gameroom -> GameInfo -> IO ReserveInfo
reserveSeat gameroom gameInfo = do
  a <- arimaaPost gameroomUrl [("action", "reserveseat")
                              ,("sid", sid gameroom)
                              ,("gid", gid gameInfo)
                              ,("role", case role gameInfo of Gold -> "w"; Silver -> "b")
                              ,("rated", "0")
                              ]
  case getFields ["gid", "gsurl", "tid"] a of
    [a,b,c] -> return $ ReserveInfo a b c

reserveView :: Gameroom -> String -> IO ReserveInfo
reserveView gameroom gid = do
  a <- arimaaPost gameroomUrl [("action", "reserveseat")
                              ,("sid", sid gameroom)
                              ,("gid", gid)
                              ,("role", "v")
                              ]
  case getFields ["gid", "gsurl", "tid"] a of
   [a,b,c] -> return $ ReserveInfo a b c

--cancelGame

--findGameId

logout :: Gameroom -> IO ()
logout gameroom = const () <$> arimaaPost gameroomUrl [("action", "logout"), ("sid", sid gameroom)]

----------------------------------------------------------------

sit :: Gameroom -> ReserveInfo -> IO (PlayInfo, Maybe Colour, Maybe Colour)
sit gameroom (ReserveInfo{gsurl,tid}) = do
  a <- arimaaPost gsurl [("action", "sit"), ("tid", tid), ("grid", grid gameroom), ("rated", "0")]
  let f s | elem s ["w", "g"] = Just Gold
          | elem s ["b", "s"] = Just Silver
          | otherwise = Nothing
  case getFields ["sid", "side", "role"] a of
    [a,b,c] -> return ((gsurl, a), f b, f c)
