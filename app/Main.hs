module Main where

import Cli
import Data.Maybe (fromMaybe)
import qualified Day1
import qualified Day2

main :: IO ()
main = do
    cli <- parseCli
    let (part1, part2) = case day cli of
            1 -> (Day1.part1, Day1.part2)
            2 -> (Day2.part1, Day2.part2)
            n -> error $ "unsupported day: " ++ show n
        defaultInput = "data/day" ++ show (day cli) ++ ".txt"
    contents <- readFile $ fromMaybe defaultInput (file cli)

    case part cli of
        One -> part1 contents
        Two -> part2 contents
        Both -> part1 contents >> part2 contents
