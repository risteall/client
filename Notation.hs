module Notation where

import Data.List.Split
import Data.List
import Data.Bifunctor
import Data.Char
import Data.Maybe
import Data.Array.IArray
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Functor.Identity
import Data.Functor.Constant

import Base

data Notation = Long | Compressed | SilverMitt deriving (Show, Read)

showMove :: Notation -> Board -> Colour -> Move -> String
showMove Long = \_ _ -> moveToString
showMove Compressed = \_ _ -> compressed
showMove SilverMitt = sm

----------------------------------------------------------------

compressed :: Move -> String
compressed (Move l) = intercalate " " $ map g $ foldr f [] l
  where
    f s [] = [[s]]
    f s@(p,sq,dir) ((s'@(p',sq',_):x):y) | p == p' && maybe False ((== Just sq') . destination sq) dir = (s:s':x) : y
                                         | otherwise = [s] : (s':x) : y
    g l@((p,sq,_):_) = pieceToChar p : squareToString sq ++ map (maybe 'x' dirToChar . _3) l

----------------------------------------------------------------

unpad (_, sq, Just d) = Step sq d

atomise :: Board -> Colour -> Move -> [([PaddedStep], [(Piece, Square)])]
atomise board player (Move l) = f $ g l
  where
    g :: [PaddedStep] -> [(PaddedStep, [(Piece, Square)])]
    g = foldl' g' []
    g' a (p, sq, Nothing) = init a ++ [second (++ [(p, sq)]) (last a)]
    g' a s = a ++ [(s, [])]
    f :: [(PaddedStep, [(Piece, Square)])] -> [([PaddedStep], [(Piece, Square)])]
    f = f' [] board
    f' a b [] = a
      -- horrible horrible code, fix it
    f' a b ((s1,c1):(s2,c2):rest) | legalDrag b player (unpad s1) (unpad s2) = f' (a ++ [([s1,s2], c1 ++ c2)]) (fromJust (playMove b (Move [s1, s2]))) rest
    f' a b ((s,c):rest) = f' (a ++ [([s], c)]) (fromJust (playMove b (Move [s]))) rest

stepToArrow :: PaddedStep -> (Piece, Square, Maybe Square)
stepToArrow (p, sq, d) = (p, sq, d >>= destination sq)

_1 (x,_,_) = x
_2 (_,x,_) = x
_3 (_,_,x) = x

expressions :: Board -> Colour -> Move -> [Expr Arr]
expressions board player = map (uncurry Expr . partition ((== player) . fst . _1))
                           . filter (not . null)
                           . map (filter (\(_,source,dest) -> dest /= Just source))
                           . foldl' (\exprs (steps, captures) -> foldl' addCapture (f exprs steps) captures) []
                           . atomise board player
  where
    f :: [[(Piece, Square, Maybe Square)]] -> [PaddedStep] -> [[(Piece, Square, Maybe Square)]]
    f exprs steps = case split (whenElt _2) $ map (flip g steps) exprs of
      [_] -> exprs ++ [map stepToArrow steps]
      x@(_:[(e,_,[])]:_) -> map _1 $ concat x
      l1:[(e,_,[s])]:l2 -> case split (whenElt _2) $ map (flip g [s]) es2 of
          [_] -> es1 ++ (e ++ [stepToArrow s]) : es2
          l3:[(e',_,_)]:l4 -> es1 ++ (e ++ e') : map _1 (l3 ++ concat l4)
        where es1 = map _1 l1
              es2 = map _1 $ concat l2
    g expr = foldl' h (expr, False, [])
    h (e, b, l) s = case exprPlusStep s e of
      (_, False) -> (e, b, s:l)
      (e', True) -> (e', True, l)
    exprPlusStep :: PaddedStep -> [(Piece, Square, Maybe Square)] -> ([(Piece, Square, Maybe Square)], Bool)
    exprPlusStep s expr = second or $ unzip $ map (arrowPlusStep s) expr
    arrowPlusStep (p', sq', dir) arr@(p, sq, dest) | p == p' && dest == Just sq' = ((p, sq, dir >>= destination sq'), True)
                                                   | otherwise = (arr, False)
    addCapture :: [[(Piece, Square, Maybe Square)]] -> (Piece, Square) -> [[(Piece, Square, Maybe Square)]]
    addCapture exprs cap@(p,sq) = case second or $ unzip $ map (second or . unzip . map (arrPlusCap cap)) exprs of
      (_, False) -> exprs ++ [[(p, sq, Nothing)]]
      (exprs', True) -> exprs'
    arrPlusCap (p', sq') arr@(p, sq, dest) | p == p' && dest == Just sq' = ((p, sq, Nothing), True)
                                           | otherwise = (arr, False)

----------------------------------------------------------------

--testB = stringToBoard "r rr r/ rdc drr/ hRE/ rh3cH/2e4m/ H C2C/RRM DRRR/R2D R"

type Arr = (Piece, Square, Maybe Square)

data Expr a = Expr [a] [a] deriving Functor

exprToArrows (Expr a b) = a ++ b

setListElem :: [a] -> Int -> a -> [a]
setListElem l n x = take n l ++ x : drop (n+1) l

data L =  L {unL :: forall f b. Functor f => (b -> f b) -> Expr b -> f (Expr b)}

lenses :: Expr a -> [L]
lenses (Expr friendly enemy) = map enemyLens (reverse [0 .. length enemy - 1])
                           ++ map friendlyLens (reverse [0 .. length friendly - 1])
  where
    friendlyLens n = L $ \f (Expr x y) -> (\a -> Expr (setListElem x n a) y) <$> f (x !! n)
    enemyLens n = L $ \f (Expr x y) -> (\a -> Expr x (setListElem y n a)) <$> f (y !! n)

get l = getConstant . l Constant
set l x = runIdentity . l (const (Identity x))

destOmissible :: Expr Arr -> Expr (Arr, Bool)
destOmissible e@(Expr [_] [_]) = (\a@(p,_,d) -> (a, isJust d && snd p /= 0)) <$> e
destOmissible e = (,False) <$> e

pieceList :: Board -> Piece -> [Square]
pieceList b p = [sq | (sq, Just p') <- assocs b, p == p']

taxicab (a,b) (c,d) = abs (a-c) + abs (b-d)

squaresWithin n sq = filter ((<= n) . taxicab sq) $ range boardRange

candidateArrows :: Board -> Colour -> Arr -> Bool -> [Arr]
candidateArrows b player (p, _, _) True
  = [(p, source', Just dest') | source' <- pieceList b p
                              ,dest' <- squaresWithin (div stepsPerMove (if fst p == player then 1 else 2)) source']
candidateArrows b player (p, _, dest) False = [(p, sq, dest) | sq <- pieceList b p, f sq]
  where
    n = div stepsPerMove (if fst p == player then 1 else 2)
    f sq = case dest of
      Just d -> taxicab sq d <= n
      Nothing -> any (\d -> taxicab sq d <= n) trapSquares

arrLength :: Arr -> Int
arrLength (_, sq, dest) = minimum [taxicab sq d | d <- maybe trapSquares (:[]) dest]

arrowSquares :: Arr -> [Square]
arrowSquares (_, (a,b), dest) = do
  (c, d) <- maybe trapSquares (:[]) dest
  range ((min a c, min b d), (max a c, max b d))

minSteps :: Expr Arr -> Int
minSteps (Expr friendly []) = sum $ map arrLength friendly
minSteps (Expr [] enemy) = let n = sum $ map arrLength enemy in 2 * n + mod n 2
minSteps (Expr friendly enemy) = maximum [2 * (length friendly + length enemy - 1), 2*n, n + sum (map f friendly)]
  where
    n = sum $ map arrLength enemy
    f (_, sq, dest) = minimum [taxicab sq x + taxicab x d
                              | e <- enemy
                              , x <- arrowSquares e
                              , d <- maybe trapSquares (:[]) dest
                              ]

showArrow :: (Arr, String, String) -> String
showArrow ((p,_,_), origin, dest) = (if snd p == 0 then "" else toUpper (pieceToChar p) : []) ++ origin ++ dest

showExpr :: Expr String -> String
showExpr (Expr friendly enemy) = intercalate "," friendly
                                 ++ if null enemy
                                      then ""
                                      else '/' : intercalate "," enemy

_2l f (a,b,c) = (a,,c) <$> f b
_3l f (a,b,c) = (a,b,) <$> f c

disambiguate :: (Piece -> Bool) -> Expr (Arr, Bool) -> [Expr Arr] -> Expr (Arr, String, String)
disambiguate omitOrigin expr exprs = fst $ foldl' g (foldl' f (initExpr, exprs) ls) ls
  where
    ls = lenses expr
    initExpr :: Expr (Arr, String, String)
    initExpr = (\(a,_) -> (a, undefined, undefined)) <$> expr
    destString (p, sq, dest) destOmissible dests
      | destOmissible && 1 == length dests = ""
      | otherwise = maybe "x" squareToString dest
    originString (p, sq, dest) origins
      | (omitOrigin p || 1 == length origins) && not (snd p == 0 && isNothing dest) = ""
      | 1 == length (filter ((== fst sq) . fst) origins) = toEnum (fromEnum 'a' + fst sq) : []
      | 1 == length (filter ((== snd sq) . snd) origins) = show (8 - snd sq)
      | otherwise = squareToString sq ++ "-"
    f (e, es) (L l) = (set (l . _3l)
                           (destString a b (nub (map (_3 . get l) es)))
                           e,
                       filter ((== _3 a) . _3 . get l) es
                      )
      where (a, b) = get l expr
    g (e, es) (L l) = (set (l . _2l)
                           (originString a (nub (map (_2 . get l) es)))
                           e,
                       filter ((== _2 a) . _2 . get l) es
                      )
      where (a, _) = get l expr

sm :: Board -> Colour -> Move -> String
sm board player move = intercalate " "
                       $ map (showExpr . fmap showArrow)
                       $ zipWith (disambiguate omitOrigin)
                                 exprs' candidates
  where
    exprs = expressions board player move
    pieceCount :: Map Piece Int
    pieceCount = foldl' (\m (p,_,_) -> Map.insertWith (+) p 1 m) Map.empty (exprs >>= exprToArrows)
    exprs' = map destOmissible exprs
    mins :: [([(Expr Arr, Int)], Int)]
    mins = map (\e -> (\l -> (l, minimum (map snd l)))
                      $ map ((\e -> (e, minSteps e)) . fmap fst)
                      $ foldl' (\l (lens,f) -> l >>= lens f)
                               [e]
                               (map (,(\(a,b) -> map (,b) $ candidateArrows board player a b)) (map unL $ lenses e)))
               exprs'
    spare = stepsPerMove - sum (map snd mins)
    candidates = map (\(l, n) -> [e | (e, m) <- l, m <= n + spare]) mins
    origins :: Map Piece Int
    origins = length . nub <$> foldl' (\m (p, sq, _) -> Map.insertWith (++) p [sq] m)
                                      Map.empty
                                      (concat candidates >>= exprToArrows)
    omitOrigin p = Map.findWithDefault 0 p pieceCount == Map.findWithDefault 0 p origins
