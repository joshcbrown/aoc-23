module Day6 (part1, part2) where

import Common (runPartWith)
import Control.Applicative (Alternative (many))
import Control.Arrow
import Data.Attoparsec.Text
import Data.Char (digitToInt)
import qualified Data.Text as T

type Time = Int
type Distance = Int

labelledSpaceSeparated :: T.Text -> Parser a -> Parser [a]
labelledSpaceSeparated label p = string label *> many (many (char ' ') *> p)

part1Line :: T.Text -> Parser [Int]
part1Line = flip labelledSpaceSeparated decimal

file :: Parser a -> Parser b -> Parser (a, b)
file line1 line2 = (,) <$> (line1 <* endOfLine) <*> (line2 <* many space <* endOfInput)

possibleDists :: Time -> [Distance]
possibleDists t = map (\t' -> (t - t') * t') [1 .. t - 1]

compute :: ([Time], [Distance]) -> Int
compute =
    product
        . uncurry (zipWith (\distances best -> length $ filter (> best) distances))
        . first (map possibleDists)

part1 :: String -> IO ()
part1 = runPartWith (file (part1Line "Time:") (part1Line "Distance:")) compute

digitsToInt :: [Int] -> Int
digitsToInt = foldl (\acc n -> 10 * acc + n) 0

part2Line :: T.Text -> Parser [Int]
part2Line label =
    (: []) . digitsToInt . map digitToInt
        <$> labelledSpaceSeparated label digit

part2 :: String -> IO ()
part2 = runPartWith (file (part2Line "Time:") (part2Line "Distance:")) compute
