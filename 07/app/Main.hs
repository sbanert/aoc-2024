module Main where

import qualified Data.Set as S

main :: IO ()
main = interact solve

solve :: String -> String
solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 = show . sum . map head . filter possible . map (map (read :: String -> Integer) . words . filter (/= ':')) . lines

possible :: [Integer] -> Bool
possible (res:n:ns) = res `S.member` vals where
  vals = foldl (\old a -> S.map (+a) old `S.union` S.map (*a) old) (S.singleton n) ns
possible _ = undefined

possible' :: [Integer] -> Bool
possible' (res:n:ns) = res `S.member` vals where
  vals = foldl (\old a -> S.map (+a) old `S.union` S.map (*a) old `S.union` S.map (read . (++ show a) . show) old) (S.singleton n) ns
possible' _ = undefined

part2 :: String -> String
part2 = show . sum . map head . filter possible' . map (map (read :: String -> Integer) . words . filter (/= ':')) . lines
