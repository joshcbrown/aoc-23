module Main where

import Cli
import Data.Maybe (fromMaybe)
import qualified Day1

main :: IO ()
main = do
    cli <- parseCli
    let (part1, part2) = case day cli of
            1 -> (Day1.part1, Day1.part2)
            n -> error $ "unsupported day: " ++ show n
    let f = fromMaybe ("data/day" ++ show (day cli) ++ ".txt") (file cli)
    case part cli of
        One -> part1 f
        Two -> part2 f
        Both -> part1 f >> part2 f
