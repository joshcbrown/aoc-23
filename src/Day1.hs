module Day1 (part1, part2) where

import Control.Applicative (many, (<|>))
import Control.Monad (void)
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import Data.Char (isAlpha)
import Data.Functor (($>))
import qualified Data.Text as T
import Prelude hiding (take, takeWhile)

letters :: Parser ()
letters = void $ takeWhile isAlpha

singleDigit :: Parser Int
singleDigit = read . (: []) <$> digit

toInt :: [Int] -> Int
toInt = sum . zipWith (\i x -> (10 ^ i) * x) [0 ..] . reverse

foo :: [Int] -> Int
foo xs = (head xs * 10) + last xs

pLetters :: Parser Int -> Parser Int
pLetters p = p <|> (letter *> pLetters p)

pNumbers :: Parser Int -> Parser [Int]
pNumbers pDigit = many $ do
    i <- many $ pLetters pDigit
    letters *> endOfLine
    pure $ foo i

common :: Parser Int -> FilePath -> IO ()
common pDigit f = (putStrLn . either id (show . sum) . parseOnly (pNumbers pDigit) . T.pack) =<< readFile f

part1 :: FilePath -> IO ()
part1 = common singleDigit

singleDigitSpelled :: Parser Int
singleDigitSpelled = singleDigit <|> spelled
  where
    digitStrings = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    spelled = choice (zipWith (\n s -> lookAhead (string s) *> take (T.length s - 1) $> n) [1 ..] digitStrings)

part2 :: FilePath -> IO ()
part2 = common singleDigitSpelled
