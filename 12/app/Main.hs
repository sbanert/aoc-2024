module Main where

import Data.Array
import Data.List (transpose)
import qualified Data.Set as S

main :: IO ()
main = interact solve

solve :: String -> String
solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 = show . sum . map cost . regions . readInput

part2 :: String -> String
part2 = show . sum . map cost' . regions . readInput

readInput :: String -> Array (Int, Int) Char
readInput s = listArray ((0, 0), (w - 1, h - 1)) (concat ls) where
  ls = transpose (lines s)
  w = length (head ls)
  h = length ls

area :: S.Set (Int, Int) -> Int
area = S.size

perimeter :: S.Set (Int, Int) -> Int
perimeter s = sum $ map (S.size . S.filter (not . flip S.member s) . neighbours) (S.toList s)

cost :: S.Set (Int, Int) -> Int
cost s = area s * perimeter s

cost' :: S.Set (Int, Int) -> Int
cost' s = area s * numSides s

above :: (Int, Int) -> (Int, Int)
above (x, y) = (x, y + 1)

right :: (Int, Int) -> (Int, Int)
right (x, y) = (x + 1, y)

numSides :: S.Set (Int, Int) -> Int
numSides s = sum $ map (\sq -> S.size $ S.filter (\n -> not (n `S.member` s) && 
                                                    ((snd n /= snd sq) || (above n `S.member` s) || not (above sq `S.member` s)) &&
                                                    ((fst n /= fst sq) || (right n `S.member` s) || not (right sq `S.member` s))) $ neighbours sq) (S.toList s)

regions :: Array (Int, Int) Char -> [S.Set (Int, Int)]
regions m = regionsHelper (S.fromList $ indices m) where
  regionsHelper r
    | S.null r = []
    | otherwise = let reg = extractRegion S.empty r in reg:regionsHelper (r S.\\ reg)
  extractRegion acc s
    | acc == S.empty = extractRegion (S.singleton (S.findMin s)) s
    | (m ! S.findMin acc) `S.member` S.map (m !) (allNeighbours acc `S.intersection` S.fromList (indices m))
      = extractRegion (acc `S.union` S.filter ((== m ! S.findMin acc) . (m !)) (allNeighbours acc `S.intersection` S.fromList (indices m))) s
    | otherwise = acc

neighbours :: (Int, Int) -> S.Set (Int, Int)
neighbours (x, y) = S.fromList [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

allNeighbours :: S.Set (Int, Int) -> S.Set (Int, Int)
allNeighbours s = S.unions (S.map neighbours s) S.\\ s
