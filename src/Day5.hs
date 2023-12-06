module Day5 (
    part1,
    part2,
) where

import Control.Arrow (Arrow (first, second))
import Data.Bifunctor (bimap)
import Data.List (uncons)
import Data.List.Split (splitWhen)
import Data.Maybe (fromJust)

firstLine :: [String] -> [Int]
firstLine = map read . words . drop 7 . head

mapping :: [String] -> [[Int]]
mapping = map (map read . words) . tail

parse :: String -> ([Int], [[[Int]]])
parse = bimap firstLine (map mapping) . fromJust . uncons . splitWhen (== "") . lines

compute :: [Int] -> [[Int]] -> [Int]
compute =
    foldl
        ( \seed [destStart, sourceStart, range] ->
            map
                (\s -> if sourceStart <= s && s < sourceStart + range then destStart + (s - sourceStart) else s)
                seed
        )

part1 :: String -> IO ()
part1 = print . uncurry (foldl compute) . parse

part2 :: String -> IO ()
part2 = undefined
