module Main where

import Data.Array
import Data.List (sort, transpose)

main :: IO ()
main = interact solve

solve :: String -> String
solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 = show . getXmas . getArray

getArray :: String -> Array (Int, Int) Char
getArray s = listArray ((0, 0), (w - 1, h - 1)) (concat ls) where
  ls = transpose (lines s)
  w = length (head ls)
  h = length ls

getXmas :: Array (Int, Int) Char -> Int
getXmas a = sum $ map (numXmasFromHere a) (indices a)

numXmasFromHere :: Array (Int, Int) Char -> (Int, Int) -> Int
numXmasFromHere a i = length . filter (== "XMAS") . map (map (a !)) . filter (all (inRange (bounds a))) $ map (take 4 . goDirection i) directions

goDirection :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
goDirection (x, y) (dx, dy) = (x, y):goDirection (x + dx, y + dy) (dx, dy)

part2 :: String -> String
part2 = show . getMasMas . getArray

numMasMasFromHere :: Array (Int, Int) Char -> (Int, Int) -> Int
numMasMasFromHere a (x, y)
  | a ! (x, y) /= 'A' || not (all (inRange (bounds a)) [(x + 1, y + 1), (x - 1, y + 1), (x - 1, y - 1), (x + 1, y - 1)]) = 0
  | sort [a ! (x + 1, y + 1), a ! (x - 1, y - 1)] == "MS" && sort [a ! (x + 1, y - 1), a ! (x - 1, y + 1)] == "MS" = 1
  | otherwise = 0

getMasMas :: Array (Int, Int) Char -> Int
getMasMas a = sum $ map (numMasMasFromHere a) (indices a)

directions :: [(Int, Int)] 
directions = [(1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)]
