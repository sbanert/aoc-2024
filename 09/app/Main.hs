module Main where

import Data.Char (isDigit)
import Data.Array

data Memtype = Free | Occupied Int deriving (Show, Eq)

isOccupied :: Memtype -> Bool
isOccupied Free = False
isOccupied (Occupied _) = True

main :: IO ()
main = interact solve

solve :: String -> String
solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 = show . checksum . getMap . getInput . filter isDigit

part2 :: String -> String
part2 s = show $ checksum' 0 0 newMap where
-- part2 s = show newMap where
  inp = getInput $ filter isDigit s
  occRevOrder = reverse $ filter (isOccupied . fst) inp
  newMap = foldl shuffle inp occRevOrder

checksum' :: Int -> Int -> [(Memtype, Int)] -> Int
checksum' _ acc [] = acc
checksum' p acc ((Free, s):r) = checksum' (p + s) acc r
checksum' p acc ((Occupied i, s):r) = checksum' (p + s) (acc + i * s * p + (i * (s - 1) * s) `div` 2) r

shuffle :: [(Memtype, Int)] -> (Memtype, Int) -> [(Memtype, Int)]
shuffle [] _ = []
shuffle ((Occupied i, s):r) (Occupied i', s')
  | i == i' = (Occupied i, s):r
  | otherwise = (Occupied i, s):shuffle r (Occupied i', s')
shuffle ((Free, s):r) (Occupied i', s')
  | s > s' = (Occupied i', s'):(Free, s - s'):removeId i' r
  | s == s' = (Occupied i', s'):removeId i' r
  | otherwise = (Free, s):shuffle r (Occupied i', s')
shuffle _ (Free, _) = undefined

removeId :: Int -> [(Memtype, Int)] -> [(Memtype, Int)]
removeId _ [] = []
removeId i ((Free, s):r) = (Free, s):removeId i r
removeId i ((Occupied i', s):r)
  | i == i' = (Free, s):r
  | otherwise = (Occupied i', s):removeId i r

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
