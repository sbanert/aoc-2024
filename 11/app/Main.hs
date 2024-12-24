module Main where

import qualified Data.Map.Strict as M

main :: IO ()
main = interact solve

solve :: String -> String
solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 = show . length . rep 25 (concatMap blink) . readInput

part2 :: String -> String
part2 = show . sum . rep 75 (concatMap' blink) . toMap . readInput

concatMap' :: (Integer -> [Integer]) -> M.Map Integer Integer -> M.Map Integer Integer
concatMap' f = M.foldrWithKey (\k a m' -> foldl (flip (M.alter (incrBy a))) m' (f k)) M.empty

readInput :: String -> [Integer]
readInput = map read . words

toMap :: [Integer] -> M.Map Integer Integer
toMap = foldl (flip (M.alter incr)) M.empty

incr :: Maybe Integer -> Maybe Integer
incr = incrBy 1

incrBy :: Integer -> Maybe Integer -> Maybe Integer
incrBy k Nothing = Just k
incrBy k (Just n) = Just (n + k)

blink :: Integer -> [Integer]
blink 0 = [1]
blink n | even (length ds) = [undigits n2, undigits n1]
        | otherwise = [2024 * n]
  where
    ds = digits n
    nds = length (digits n)
    (n1, n2) = splitAt (nds `div` 2) ds

rep :: Int -> (a -> a) -> a -> a
rep 0 _ x = x
rep n f x = rep (n - 1) f (f x)

digits :: Integer -> [Integer]
digits 0 = []
digits n = (n `mod` 10):digits (n `div` 10)

undigits :: [Integer] -> Integer
undigits [] = 0
undigits (d:r) = d + 10 * undigits r
