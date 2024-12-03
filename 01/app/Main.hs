module Main where
import Data.List (sort)
import Data.Bifunctor (bimap)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

main :: IO ()
main = interact solve

makePair :: [String] -> (Int, Int)
makePair [a, b] = (read a, read b)
makePair _ = undefined

solve :: String -> String
solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 = show . sum . uncurry (zipWith (\a b -> abs(a - b))) 
  . bimap sort sort
  -- . (\(a, b) -> (sort a, sort b))
  . unzip . map (makePair . words) . lines

part2 :: String -> String
part2 s = show (sum scores) where
  pairs = map (makePair . words) $ lines s
  left_list = map fst pairs
  right_list = map snd pairs
  rle_right = foldl (flip (M.alter incr)) M.empty right_list
  scores = map (\a -> a * fromMaybe 0 (M.lookup a rle_right)) left_list


incr :: Maybe Int -> Maybe Int
incr Nothing = Just 1
incr (Just n) = Just (n+1)
