module Main where

main :: IO ()
main = interact (\s -> unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s, "Part 2': " ++ part2' s])

isSafe :: [Int] -> Bool
isSafe s = isSafeIncr s || isSafeDecr s

isSafeIncr :: [Int] -> Bool
isSafeIncr [] = True
isSafeIncr [_] = True
isSafeIncr (a1:a2:r) = a2 >= a1 + 1 && a2 <= a1 + 3 && isSafeIncr (a2:r)
 
isSafeDecr :: [Int] -> Bool
isSafeDecr [] = True
isSafeDecr [_] = True
isSafeDecr (a1:a2:r) = a2 <= a1 - 1 && a2 >= a1 - 3 && isSafeDecr (a2:r)

isAlmostSafeIncr :: [Int] -> Bool
isAlmostSafeIncr [] = True
isAlmostSafeIncr [_] = True
isAlmostSafeIncr [_, _] = True
isAlmostSafeIncr (a1:a2:a3:r) = (a2 >= a1 + 1 && a2 <= a1 + 3 && isAlmostSafeIncr (a2:a3:r))
  || isSafeIncr (a1:a3:r)

isAlmostSafeDecr :: [Int] -> Bool
isAlmostSafeDecr [] = True
isAlmostSafeDecr [_] = True
isAlmostSafeDecr [_, _] = True
isAlmostSafeDecr (a1:a2:a3:r) = (a2 <= a1 - 1 && a2 >= a1 - 3 && isAlmostSafeDecr (a2:a3:r))
  || isSafeDecr (a1:a3:r)

subsequences :: [a] -> [[a]]
subsequences [] = []
subsequences [_] = [[]]
subsequences (a:r) = r : map (a:) (subsequences r)

isAlmostSafe' :: [Int] -> Bool
isAlmostSafe' s = isSafe (tail s) || isAlmostSafeIncr s || isAlmostSafeDecr s

isAlmostSafe :: [Int] -> Bool
isAlmostSafe s = isSafe s || any isSafe (subsequences s)

part1 :: String -> String
part1 = show . length . filter id . map (isSafe . map read . words) . lines

part2 :: String -> String
part2 = show . length . filter id . map (isAlmostSafe . map read . words) . lines

part2' :: String -> String
part2' = show . length . filter id . map (isAlmostSafe' . map read . words) . lines
