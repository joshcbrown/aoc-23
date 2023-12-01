module Day1 (part1, part2) where

import Control.Applicative (many, (<|>))
import Control.Monad (void)
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import Data.Char (digitToInt, isAlpha)
import Data.Functor (($>))
import qualified Data.Text as T
import Prelude hiding (take, takeWhile)

letters :: Parser ()
letters = void $ takeWhile isAlpha

singleDigit :: Parser Int
singleDigit = digitToInt <$> digit

-- toInt :: [Int] -> Int
-- toInt = sum . zipWith (\i x -> (10 ^ i) * x) [0 ..] . reverse

pLetters :: Parser Int -> Parser Int
pLetters p = p <|> (letter *> pLetters p)

pNumbers :: Parser Int -> Parser Int
pNumbers pDigit = (fmap sum . many) $ do
    digits <- many $ pLetters pDigit
    letters *> endOfLine
    pure $ head digits * 10 + last digits

runPartWith :: Parser Int -> FilePath -> IO ()
runPartWith pDigit f =
    readFile f
        >>= ( putStrLn
                . either id show
                . parseOnly (pNumbers pDigit)
                . T.pack
            )

part1 :: FilePath -> IO ()
part1 = runPartWith singleDigit

singleDigitSpelled :: Parser Int
singleDigitSpelled = singleDigit <|> spelledDigit
  where
    digitStrings = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    -- spelled digits can overlap by 1 letter so we can't consume all the input
    spelledDigitN n spelling = (lookAhead (string spelling) *> take (T.length spelling - 1)) $> n
    spelledDigit = choice $ zipWith spelledDigitN [1 ..] digitStrings

part2 :: FilePath -> IO ()
part2 = runPartWith singleDigitSpelled
