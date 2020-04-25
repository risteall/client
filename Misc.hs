module Misc where

import Text.Printf
import Generics.Deriving
import Graphics.UI.Gtk
import GHC.TypeLits
import Data.Kind
import Data.Proxy
import Control.Monad

import WidgetValue

timeBits :: Int -> [Int]
timeBits n = [d, h, m, s]
  where
    (m', s) = divMod n 60
    (h', m) = divMod m' 60
    (d, h) = divMod h' 24

showTreeDuration :: Int -> String
showTreeDuration n | n < 0 = show n ++ "s"
showTreeDuration 0 = "0"
showTreeDuration n = concat $ zipWith (\n x -> if n == 0 then "" else show n ++ [x])
                                      (timeBits n) "dhms"

showClockDuration' :: Int -> (String, String)
showClockDuration' n | n < 0 = ("", show n ++ "s")
showClockDuration' n = case timeBits n of
  [d,h,m,s] | d > 0 -> (show d ++ "d" ++ show h ++ "h", t)
            | h > 0 -> (show h ++ "h", t)
            | otherwise -> ("", t)
    where t = printf "%02d:%02d" m s

showClockDuration :: Int -> String
showClockDuration = uncurry (++) . showClockDuration'

----------------------------------------------------------------

data PieceSet = TwoD | ThreeD deriving (Read, Show, Eq, Ord)

instance WidgetValue PieceSet HBox where
  makeWidget = enumWidget [(TwoD, "2D"), (ThreeD, "3D")]

----------------------------------------------------------------

mapAccumLM :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapAccumLM f x l = foldM (\(a,cs) g -> fmap (\(a',c) -> (a',cs++[c])) (g a)) (x,[]) (map (flip f) l)

----------------------------------------------------------------

-- BoundedNat caused compiler panic with Template Haskell

-- data BoundedNat :: Nat -> Type where
--     BoundedNat :: {getBoundedNat :: Integer} -> BoundedNat n
--   deriving (Read, Show)

-- mkBoundedNat :: forall n. KnownNat n => Integer -> Maybe (BoundedNat n)
-- mkBoundedNat m | m >= 0 && m <= natVal (Proxy :: Proxy n) = Just (BoundedNat m)
--                | otherwise = Nothing

-- instance forall n. KnownNat n => WidgetValue (BoundedNat n) SpinButton where
--   makeWidget = do
--     b <- spinButtonNewWithRange 0 (fromIntegral (natVal (Proxy :: Proxy n))) 1
--     return (b, mkBoundedNat . round <$> spinButtonGetValue b, spinButtonSetValue b . fromIntegral . getBoundedNat)

-- instance forall n. KnownNat n => WidgetValue (Maybe (BoundedNat n)) HBox where
--   makeWidget = maybeWidget
