module Main where

import Data.Char (isDigit)
import Data.Array

data Memtype = Free | Occupied Int deriving (Show, Eq)

main :: IO ()
main = interact solve

solve :: String -> String
solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 = show . checksum . getMap . getInput . filter isDigit

part2 :: String -> String
part2 = const ""

getInput :: String -> [(Memtype, Int)]
getInput = getBlock 0

getBlock :: Int -> String -> [(Memtype, Int)]
getBlock _ [] = []
getBlock i (h:t) = (Occupied i, read [h]):getFree (i + 1) t

getFree :: Int -> String -> [(Memtype, Int)]
getFree _ [] = []
getFree i (h:t) = (Free, read [h]):getBlock i t

getMap :: [(Memtype, Int)] -> Array Int Memtype
getMap l = listArray (0, len - 1) (unRle l) where
  len = sum $ map snd l

unRle :: [(a, Int)] -> [a]
unRle [] = []
unRle ((c, n):t) = replicate n c ++ unRle t

checksum :: Array Int Memtype -> Int
checksum a = checksumAux 0 (bounds a) where
  checksumAux acc (l, r) = if l > r then acc else case (a ! l, a ! r) of
     (Free, Free) -> checksumAux acc (l, r - 1)
     (Free, Occupied ir) -> checksumAux (acc + l * ir) (l + 1, r - 1)
     (Occupied il, _) -> checksumAux (acc + l * il) (l + 1, r)
