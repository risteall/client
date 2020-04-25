module Shadow where

import Data.Array.IArray
import Data.Maybe
import Data.List

import Base

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
