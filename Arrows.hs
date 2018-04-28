module Arrows where

import Graphics.Rendering.Cairo
import Data.List
import Control.Monad
import Control.Applicative
import qualified Data.Function as Function
import Data.Bifunctor

import Base

equivalenceClasses :: (a -> a -> Bool) -> [a] -> [[a]]
equivalenceClasses rel [] = []
equivalenceClasses rel (x:xs) = case split [] [x] xs of
    (u, v) -> u : equivalenceClasses rel v
  where
    split a [] c = (a, c)
    split a (x:xs) c = case partition (rel x) c of
      (u, v) -> split (x:a) (xs++u) v

overlappingArrows :: Arrow -> Arrow -> Bool
overlappingArrows (p1,p2) (q1,q2)
  = p1 /= p2
    && q1 /= q2
    && collinear p1 p2 q1
    && collinear p1 p2 q2
    && not (and (liftA2 (<=) [p1,p2] [q1,q2]) || and (liftA2 (>=) [p1,p2] [q1,q2]))
  where collinear (x1,y1) (x2,y2) (x3,y3) = (x2-x1) * (y3-y1) - (x3-x1) * (y2-y1) == 0

drawArrowSet :: [(Arrow, Render ())] -> Render ()
drawArrowSet = mapM_ f . concatMap addOffsets
                       . equivalenceClasses (overlappingArrows `Function.on` fst)
                       . filter (uncurry (/=) . fst)
  where
    f ((arr, action), i) = do {action; drawArrow arr i}
    g (arr, action) arrs = ((arr, action), offset) : arrs
      where Just offset = find (\i -> not (any (\((a,act),offs) -> overlappingArrows arr a && offs == i) arrs)) offsets
            offsets = 0 : map (\n -> if n <= 0 then 2 - n else -n) offsets
    addOffsets :: [(Arrow, Render ())] -> [((Arrow, Render ()), Int)]
    addOffsets c = map (second (subtract x)) withOffsets
      where
        withOffsets = foldr g [] $ (sortBy (compare `Function.on` (arrowLength . fst))) c
        os = map snd withOffsets
        x = div (maximum os + minimum os) 2

arrowPath :: (Double, Double) -> (Double, Double) -> Render ()
arrowPath (u1,v1) (u2,v2) = do
  moveTo u1 v1
  lineTo u2 v2
  save
  rotate $ atan2 (v2 - v1) (u2 - u1)
  arrowHead
  restore

pathToArrow :: [Square] -> Arrow
pathToArrow p = (head p, last p)

drawPath :: [Square] -> Render ()
drawPath arr = do
    setLineWidth arrowWidth
    setLineCap LineCapRound
    setLineJoin LineJoinRound
    bendyArrow arr
    stroke
  where
    arrowPoints = map (\(x,y) -> (fromIntegral x + 0.5, fromIntegral y + 0.5)) arr
    bendyArrow [] = return ()
    bendyArrow [_] = return ()
    bendyArrow arr
      | head arr == last arr = do
          let ((x,y):_) = arrowPoints
          arc x y (0.5 - arrowWidth / 2) 0 (2 * pi)
      | straightPath arr = arrowPath (head arrowPoints) (last arrowPoints)
    bendyArrow [_,_,_] = do
      let [(x0,y0),p1,p2] = arrowPoints
      moveTo x0 y0
      curvedArrow (weightedPoint (2/3) (x0,y0) p1) (weightedPoint (1/3) p1 p2) p2
    bendyArrow [_,_,_,_] = do
      let [(x0,y0),p1,p2,p3] = arrowPoints
      moveTo x0 y0
      curvedArrow p1 p2 p3
    bendyArrow [_,_,_,_,_] = do
      let [(x0,y0),p1,p2,p3,p4] = arrowPoints
      moveTo x0 y0
      curvedArrow (weightedPoint (1/3) p1 p2) (weightedPoint (2/3) p2 p3) p4
    bendyArrow _ = return ()

weightedPoint :: Double -> (Double, Double) -> (Double, Double) -> (Double, Double)
weightedPoint t (x0,y0) (x1,y1) = ((1-t)*x0 + t*x1, (1-t)*y0 + t*y1)

findLastPoint :: (Double -> Bool) -> Double -> Double -> Maybe Double
findLastPoint f step tolerance = fmap (\x -> bisect x 1) $ find (not . f) [1, 1-step .. 0]
  where
    bisect x z | z - x <= tolerance = y
               | f y = bisect x y
               | otherwise = bisect y z
      where y = (x + z) / 2

cubic :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Double -> (Double, Double)
cubic (x0,y0) (x1,y1) (x2,y2) (x3,y3) t = (f x0 x1 x2 x3, f y0 y1 y2 y3)
  where f u0 u1 u2 u3 = u0*(1-t)^3 + 3*u1*t*(1-t)^2 + 3*u2*t^2*(1-t) + u3*t^3

curvedArrow :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Render ()
curvedArrow p1@(x1,y1) p2@(x2,y2) p3@(x3,y3) = do
  p0 <- getCurrentPoint
  curveTo x1 y1 x2 y2 x3 y3
  case findLastPoint (\t -> case cubic p0 p1 p2 p3 t of (x,y) -> sqrt ((x-x3)^2 + (y-y3)^2) < 0.3) 0.1 0.001 of
    Nothing -> return ()
    Just t -> case cubic p0 p1 p2 p3 t of
      (x,y) -> do
        save
        rotate $ atan2 (y3 - y) (x3 - x)
        arrowHead
        restore

straightPath :: [Square] -> Bool
straightPath arr = case zipWith (\(x1,y1) (x2,y2) -> (x1-x2, y1-y2)) arr (tail arr) of
  [] -> True
  (a:as) -> all (== a) as

arrowWidth = 0.15 :: Double

arrowHead :: Render ()
arrowHead = do
  relLineTo (-0.3) 0.2
  relMoveTo 0.3 (-0.2)
  relLineTo (-0.3) (-0.2)

drawArrow :: Arrow -> Int -> Render ()
drawArrow ((x1,y1), (x2,y2)) offset = do
  setLineWidth arrowWidth
  setLineCap LineCapRound
  setLineJoin LineJoinRound
  save
  let [u1,v1,u2,v2] = map ((+ 0.5) . fromIntegral) [x1,y1,x2,y2]
      a = v1 - v2
      b = u2 - u1
      d = sqrt (a^2 + b^2)
      (a',b') = if (a,b) <= (0,0) then (a,b) else (-a,-b)
      f x = x / d * 0.65 * arrowWidth * fromIntegral offset
  translate (f a') (f b')
  arrowPath (u1,v1) (u2,v2)
  restore
  stroke
