module Main where

import Data.Maybe (fromMaybe, listToMaybe)

main :: IO ()
main = interact solve

solve :: String -> String
solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 = show . sum . map cost . readInput

part2 :: String -> String
part2 = show . sum . map (cost . conversion) . readInput

readInput :: String -> [((Integer, Integer), (Integer, Integer), (Integer, Integer))]
readInput s = getMachines ls where
 ls = lines s

getMachines :: [String] -> [((Integer, Integer), (Integer, Integer), (Integer, Integer))]
getMachines [] = []
getMachines ("":r) = getMachines r
getMachines (la:lb:lp:r) = getMachine la lb lp:getMachines r
getMachines _ = undefined

getMachine :: String -> String -> String -> ((Integer, Integer), (Integer, Integer), (Integer, Integer))
getMachine la lb lp = ((read xa', read ya'), (read xb', read yb'), (read xp', read yp')) where
  ra = dropWhile (/= '+') la
  (xa', ra') = span (/= ',') (tail ra)
  ra'' = dropWhile (/= '+') ra'
  ya' = tail ra''
  rb = dropWhile (/= '+') lb
  (xb', rb') = span (/= ',') (tail rb)
  rb'' = dropWhile (/= '+') rb'
  yb' = tail rb''
  rp = dropWhile (/= '=') lp
  (xp', rp') = span (/= ',') (tail rp)
  rp'' = dropWhile (/= '=') rp'
  yp' = tail rp''

conversion :: ((Integer, Integer), (Integer, Integer), (Integer, Integer)) -> ((Integer, Integer), (Integer, Integer), (Integer, Integer))
conversion ((xa, ya), (xb, yb), (xp, yp)) = ((xa, ya), (xb, yb), (xp + 10000000000000, yp + 10000000000000))

-- Sufficient for part 1, but not for part 2:
-- cost :: ((Integer, Integer), (Integer, Integer), (Integer, Integer)) -> Integer
-- cost ((xa, ya), (xb, yb), (xp, yp)) = if null costs then 0 else minimum costs where
--   nas = takeWhile (\n -> n * xa <= xp && n * ya <= yp) [0 ..]
--   nas' = filter (\n -> yb * (xp - n * xa) == xb * (yp - n * ya)) nas
--   costs = map (\n -> 3 * n + (xp - n * xa) `div` xb) nas'
--
cost :: ((Integer, Integer), (Integer, Integer), (Integer, Integer)) -> Integer
cost ((xa, ya), (xb, yb), (xp, yp))
-- Unique solution: determinant non-zero, Cramer's rule
  | yb * xa /= xb * ya = let
                           na = (xp * yb - xb * yp) `div` (xa * yb - xb * ya)
                           nb = (xa * yp - xp * ya) `div` (xa * yb - xb * ya)
                         in if na >= 0 && nb >= 0 && na * xa + nb * xb == xp && na * ya + nb * yb == yp then 3 * na + nb else 0
-- Non-unique solution: infeasible constraints
  | yb * xa == xb * ya && xb * yp /= xp * yb = 0
-- Non-unique solution
  | yb * xa == xb * ya && xb * yp == xp * yb && ya > 3 * yb = fromMaybe 0 $ listToMaybe [3 * na + nb | nb <- [0 .. min xa ya - 1], na <- [(yp - nb * yb) `div` ya], na >= 0, nb >= 0, na * xa + nb * xb == xp, na * ya + nb * yb == yp]
  | yb * xa == xb * ya && xb * yp == xp * yb && ya <= 3 * yb = fromMaybe 0 $ listToMaybe [3 * na + nb |
                                                                                            na <- [0 .. min xb yb - 1],
                                                                                            na >= 0,
                                                                                            nb <- [(yp - na * ya) `div` yb],
                                                                                            nb >= 0,
                                                                                            na * xa + nb * xb == xp,
                                                                                            na * ya + nb * yb == yp]
    | otherwise = undefined


-- min (3 * na + nb)
-- s.t. na * xa + nb * xb = xp
--      na * ya + nb * yb = yp
--
--      nb = (xp - na * xa) / xb = (yp - na * ya) / yb
--      ----------------------------------------------
--      yb * (xp - na * xa) = xb * (yp - na * ya)

-- Unique solution: 
-- min (xp + na * (3 * xb * xa)) / xb
-- s.t. na * xa + nb * xb = xp
--      na * ya + nb * yb = yp
--      ----------------------
--      na * xa * ya + nb * xb * ya = xp * ya
--      na * xa * ya + nb * yb * xa = yp * xa
--      ----------------------
--      na * xa * ya + nb * xb * ya = xp * ya
--      nb = (yp * xa - xp * ya) / (yb * xa - xb * ya)
--      na = (xp * yb - xb * yp) / (yb * xa - xb * ya)
--
-- Non-unique solution: yb * xa = xb * ya
-- min (3 * na + nb)
-- s.t. xb * (na * ya + nb * yb) = xp * yb
--            na * ya + nb * yb  = yp
--
-- na = (yp - nb * yb) / ya
-- nb = (yp - na * ya) / yb
-- min ((3 * xp * yb) / (xb * ya) + nb * (1 - 3 * yb / ya))
-- s.t. na = (xp * yb - nb * yb * xb) / (xb * ya) >= 0
--      nb >= 0
