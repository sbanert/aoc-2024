module Main where

data SType = Key | Lock deriving Show

isKey :: SType -> Bool
isKey Key = True
isKey Lock = False

isLock :: SType -> Bool
isLock = not . isKey

main :: IO ()
main = interact solve

solve :: String -> String
solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 = show . nCombinations . keysLocks . getSchematics . lines

part2 :: String -> String
part2 = const "no Part 2"

getSchematics :: [String] -> [(SType, [Int])]
getSchematics [] = []
getSchematics ("":r) = getSchematics r
getSchematics ls = (t, p):getSchematics r where
  (s, r) = span (/= "") ls
  t = if all (== '#') (head s) then Lock else Key
  p = foldl (\acc l -> zipWith (+) acc (map toInt l)) (repeat (-1)) s

keysLocks :: [(SType, [Int])] -> ([[Int]], [[Int]])
keysLocks l = (keys, locks) where
  keys = map snd $ filter (isKey . fst) l
  locks = map snd $ filter (isLock . fst) l

nCombinations :: ([[Int]], [[Int]]) -> Int
nCombinations (keys, locks) = sum $ map (\k -> length (filter (`fits` k) locks)) keys

toInt :: Char -> Int
toInt '#' = 1
toInt '.' = 0
toInt _ = undefined

fits :: [Int] -> [Int] -> Bool
fits a = all (<= 5) . zipWith (+) a
