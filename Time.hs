module Time where

import Text.Printf

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
