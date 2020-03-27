{-# LANGUAGE LambdaCase, MultiWayIf, TupleSections, ScopedTypeVariables, NamedFieldPuns #-}

module Draw where

import Graphics.Rendering.Cairo
import Data.List
import Control.Monad
import Control.Applicative
import qualified Data.Function as Function
import Data.Bifunctor
import Graphics.UI.Gtk hiding (get, Arrow)
import Data.Array.IArray
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Reactive.Banana
import Reactive.Banana.Frameworks

import Base
import qualified Node
import Match
import Env

-- does Shadow need its own module ?
data ShadowBoard = ShadowBoard Board (Array Int Int) Int

newShadowBoard :: ShadowBoard
newShadowBoard = ShadowBoard emptyBoard (listArray (0, length pieceInfo - 1) (map snd pieceInfo)) (length pieceInfo - 1)

flipShadowSquare :: Colour -> Square -> ShadowBoard -> ShadowBoard
flipShadowSquare c (x,y) sb@(ShadowBoard b remaining current)
    | elem y (setupRows c) = case b ! (x,y) of
      Just (_, pieceType) -> ShadowBoard (b // [((x,y),Nothing)]) (remaining // [(pieceType, remaining ! pieceType + 1)]) pieceType
      Nothing -> ShadowBoard (b // [((x,y), Just (c, current))]) newRemaining (fromMaybe current maybeCurrent)
        where newRemaining = remaining // [(current, remaining ! current - 1)]
              maybeCurrent = find (\n -> newRemaining ! n /= 0) (current : reverse (indices remaining))
    | otherwise = sb

realiseShadow :: Colour -> ShadowBoard -> Array Square (Maybe (Piece, Bool))
realiseShadow c (ShadowBoard sb remaining _) = case map fst $ filter ((/= 0) . snd) (assocs remaining) of
    [n] -> solid // (map (,Just ((c,n), False)) $ filter (\sq -> sb ! sq == Nothing) setupSquares)
    _ -> solid
  where
    solid = fmap (fmap (,True)) sb
    setupSquares = (,) <$> [0 .. boardWidth - 1] <*> setupRows c

fullShadow :: ShadowBoard -> Bool
fullShadow (ShadowBoard _ remaining _) = case filter (/= 0) (elems remaining) of
  _:_:_ -> False
  _ -> True

emptyShadow :: ShadowBoard -> Bool
emptyShadow (ShadowBoard b _ _) = b == emptyBoard

----------------------------------------------------------------

genDiff :: (a -> b -> Bool) -> [a] -> [b] -> [a]
genDiff pred as bs = foldl' f as bs
  where
    f as b = case break (flip pred b) as of
      (_, []) -> as
      (l1, (_:l2)) -> l1 ++ l2


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

----------------------------------------------------------------

borderWidth = 10 :: Double

squareSize :: WidgetClass w => w -> IO Double
squareSize widget = do
  w <- widgetGetAllocatedWidth widget
  h <- widgetGetAllocatedHeight widget
  let [x,y] = zipWith (\a b -> (fromIntegral a - 2 * borderWidth) / fromIntegral b) [w,h] [boardWidth, boardHeight]
  return $ min x y

withTransform :: Double -> Double -> Double -> Render () -> Render ()
withTransform x y size action = do
  save
  translate x y
  scale size size
  action
  restore

drawEmptyBoard :: Render ()
drawEmptyBoard = do
  setSourceRGB 0.9 0.8 0.6
  forM_ trapSquares $ \(x,y) -> do
    moveTo (fromIntegral x) (fromIntegral y)
    relLineTo 0 1
    relLineTo 1 0
    relLineTo 0 (-1)
    closePath
    fill
  setSourceRGB 0.5 0.5 0.5
  (a,b) <- deviceToUserDistance 1 0
  setLineWidth (sqrt (a^2 + b^2))
  forM_ [0..8] $ \x -> do
    moveTo x 0
    lineTo x 8
    stroke
    moveTo 0 x
    lineTo 8 x
    stroke

drawLiveTraps :: Map Square Bool -> (Square -> Square) -> Render ()
drawLiveTraps lt squareMap = do
  setSourceRGB 1 0 0
  setLineWidth 0.1
  forM_ (map squareMap $ Map.keys $ Map.filter id lt) $ \(u, v) -> do
    moveTo (fromIntegral u) (fromIntegral v)
    relLineTo 0 1
    relLineTo 1 0
    relLineTo 0 (-1)
    closePath
    stroke

drawPiece :: Surface -> Double -> Render ()
drawPiece surface alpha = do
  w <- imageSurfaceGetWidth surface
  save
  scale (1 / fromIntegral w) (1 / fromIntegral w)
  setSourceSurface surface 0 0
  paintWithAlpha alpha
  restore

drawPieces :: Array Piece Surface -> Array Colour Bool -> Board -> (Square -> Square) -> Render ()
drawPieces icons visible board squareMap = forM_ (map (first squareMap) (assocs board)) $ \case
  ((x,y), Just piece@(c,_)) | visible ! c -> withTransform (fromIntegral x) (fromIntegral y) 1
                                                           $ drawPiece (icons ! piece) 1
  _ -> return ()

drawSetupPieces :: Array Piece Surface -> Colour -> Board -> ShadowBoard -> (Square -> Square) -> Render ()
drawSetupPieces icons c board shadowBoard squareMap = do
  sequence_ $ zipWith3 (\(x,y) m1 m2 -> maybe (return ())
                                              (withTransform (fromIntegral x) (fromIntegral y) 1)
                                              (m1 <|> m2))
                       (map squareMap (range boardRange))
                       (map (fmap (\p -> drawPiece (icons ! p) 1)) (elems board))
                       (map (fmap (\(p, solid) -> drawPiece (icons ! p) (if solid then 1 else 0.5)))
                            (elems (realiseShadow c shadowBoard)))

trappedPieceSize = 0.4 :: Double

trappedPieceSquares :: [(Double, Double)]
trappedPieceSquares = [(0,0), (x,0), (0,x), (x,x)]
  where x = 1 - trappedPieceSize

smallSquare :: (Double, Double) -> Maybe Int
smallSquare (x,y) = findIndex (\(u,v) -> x>=u && x<=u+trappedPieceSize && y>=v && y<=v+trappedPieceSize)
                              trappedPieceSquares

drawTrappedPiece :: Surface -> Square -> Int -> Maybe Bool -> Render ()
drawTrappedPiece surface (x,y) n question
  = when (inRange (0,3) n) $ withTransform (fromIntegral x + u) (fromIntegral y + v) trappedPieceSize $ do
    drawPiece surface (case question of Just False -> 0.5; _ -> 1)
    case question of
      Nothing -> return ()
      Just q -> do
        if q then setSourceRGB 0.9 0 0 else setSourceRGB 0 0.7 0
        te <- textExtents "?"
        save
        translate 0.1 0.1
        let x = 0.4 / textExtentsWidth te
        scale x x
        moveTo (- textExtentsXbearing te) (- textExtentsYbearing te)
        showText "?"
        restore
  where (u,v) = trappedPieceSquares !! n

drawNonsetup
    (depth :: Int)
    (board :: Board)
    (move :: Maybe Move)
    (arrows :: [Arrow])
    (liveTraps :: Map Square Bool)
    (ms :: Maybe MoveSet)
    (visible :: Array Colour Bool)
    (icons :: Array Piece Surface)
    (squareMap :: Square -> Square)
    (canvas :: DrawingArea)
    = do
  x <- liftIO $ squareSize canvas
  setSourceRGB 1 1 1
  paint
  translate borderWidth borderWidth
  scale x x

  drawEmptyBoard
  drawLiveTraps liveTraps squareMap

  drawPieces icons
             (if depth <= 2 && null arrows then listArray (Gold,Silver) (repeat True) else visible)
             (fromMaybe board (ms >>= currentMove >>= playMove board))
             squareMap

  let pathColour (Just (c,_)) _ True | not (visible ! c) = setSourceRGB 0 0.9 0
      pathColour Nothing _ True | not (visible ! Gold) || not (visible ! Silver) = setSourceRGB 0 0.9 0
      pathColour (Just (Gold,_)) solid _ = setSourceRGBA 1 0 0 (if solid then 1 else 0.5)
      pathColour (Just (Silver,_)) solid _ = setSourceRGBA 0 0 1 (if solid then 1 else 0.5)
      pathColour Nothing _ _ = setSourceRGB 0 0 0

      noInput = null arrows && not (or liveTraps)

  if | depth <= 2 && noInput -> return ()
     | noInput -> do
         let m = maybe (Move []) id move
             (straight, bendy) =  partition (straightPath . snd) $ moveToPaths m
         drawArrowSet $ map (first (bimap squareMap squareMap))
                            $ map (\(p, path) -> (pathToArrow path, pathColour (Just p) False False))
                                  straight
         forM_ bendy $ \(p, path) -> do {pathColour (Just p) False False; drawPath $ map squareMap path}

         forM_ (Map.assocs (moveToCaptureSet m))
               $ \(sq, pieces) -> zipWithM_ (\p i -> drawTrappedPiece (icons ! p) (squareMap sq) i Nothing) pieces [0..]

     | otherwise -> do
         let (straight, bendy) = case ms >>= currentMove of
               Nothing -> ([], [])
               Just m -> partition (straightPath . snd) $ filter (\((c,_),_) -> visible ! c)
                                                        $ genDiff (\(_,p) a -> pathToArrow p == a)
                                                                  (moveToPaths m)
                                                                  arrows
             straightActions = map (\(p, path) -> (pathToArrow path, pathColour (Just p) False False)) straight
             arrActions = map (\a -> (a, pathColour (board ! fst a) True True)) arrows
         drawArrowSet $ map (first (bimap squareMap squareMap)) $ arrActions ++ straightActions
         forM_ bendy $ \(p, path) -> do {pathColour (Just p) False False; drawPath $ map squareMap path}

         -- should make the correspondence between this and buttonPressCallback explicit
         let f :: Square -> [Bool] -> ([Piece], [Piece]) -> Render ()
             f trap bs (mandatory, optional) = zipWithM_ ($) (map g (filter ((visible !) . fst) mandatory)
                                                                ++ zipWith h bs optional)
                                                             [0..]
               where
                 g m i = drawTrappedPiece (icons ! m) (squareMap trap) i Nothing
                 h b o i = drawTrappedPiece (icons ! o) (squareMap trap) i (Just b)

         case ms of
           Nothing -> return ()
           Just MoveSet{currentCaptures, captures} -> sequence_ $ Map.intersectionWithKey f currentCaptures captures

drawSetup :: Colour -> Board -> ShadowBoard -> Array Piece Surface -> (Square -> Square) -> DrawingArea -> Render ()
drawSetup c board sb icons squareMap canvas = do
  x <- liftIO $ squareSize canvas
  setSourceRGB 1 1 1
  paint
  translate borderWidth borderWidth
  scale x x

  drawEmptyBoard

  drawSetupPieces icons c board sb squareMap

drawNode :: Node.SomeNode -> Behavior (ShadowBoard -> [Arrow] -> Map Square Bool -> Maybe MoveSet -> Array Colour Bool -> (Square -> Square) -> DrawingArea -> Render ())
drawNode node
  | Node.setupPhase (Just node)
  = case Node.getContentR node of
      Left regular -> pure $ \sb _ _ _ _ squareMap -> drawSetup (posToMove (Node.next regular))
                                                                (posBoard (Node.next regular))
                                                                sb (get icons) squareMap
      Right _ -> pure $ \_ _ _ _ _ _ _ -> return ()  -- shouldn't happen
  | otherwise = f <$> Node.board (Just node) <*> ((>>= either (const Nothing) Just) <$> Node.getMove node)
  where
    f board move _ as lt ms visible squareMap = drawNonsetup (Node.depth (Just node)) board move as lt ms visible (get icons) squareMap

drawSetupIcon :: Bool -> Surface -> DrawingArea -> Render ()
drawSetupIcon b s da = do
  x <- fromIntegral <$> liftIO (min <$> widgetGetAllocatedWidth da <*> widgetGetAllocatedHeight da)
  scale x x
  let borderWidth = 0.1
  when b $ do
    setLineWidth borderWidth
    moveTo (borderWidth/2) (borderWidth/2)
    lineTo (borderWidth/2) (1 - borderWidth/2)
    lineTo (1 - borderWidth/2) (1 - borderWidth/2)
    lineTo (1 - borderWidth/2) (borderWidth/2)
    closePath
    stroke
  translate borderWidth borderWidth
  z <- ((* (1 - 2*borderWidth)) . recip . fromIntegral) <$> imageSurfaceGetWidth s
  scale z z
  setSourceSurface s 0 0
  paint

drawCaptures :: Board -> Array Colour Bool -> Array Piece Surface -> DrawingArea -> Render ()
drawCaptures board visible icons canvas = do
  w <- liftIO $ widgetGetAllocatedWidth canvas
  h <- liftIO $ widgetGetAllocatedHeight canvas

  let l = concat $ reverse $ zipWith replicate (map snd pieceInfo) [0..]
      pieces = catMaybes (elems board)                              -- TODO: replace catMaybes
      captures c = map (c,) l \\ filter ((== c) . fst) pieces
      n = fromIntegral $ maximum (0 : [length (captures c) | c <- [Gold, Silver], visible ! c])
      z = fromIntegral h / 2
      overlap = if n <= 1 then 0 else max (z*0.1) ((n*z - fromIntegral w) / (n - 1))
  
  forM_ [(Gold, 0), (Silver, z)] $ \(c, y) ->
    when (visible ! c) $ zipWithM_ (\p i -> withTransform (i * (z-overlap)) y z $ drawPiece (icons ! p) 1)
                                   (captures c) [0..]

----------------------------------------------------------------
