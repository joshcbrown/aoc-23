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

pLettersAndInt :: Parser Int -> Parser Int
pLettersAndInt p = p <|> (letter *> pLettersAndInt p)

pNumbers :: Parser Int -> Parser Int
pNumbers pDigit = (fmap sum . many) $ do
    digits <- many $ pLettersAndInt pDigit
    letters *> endOfLine
    pure $ head digits * 10 + last digits

runPartWith :: Parser Int -> String -> IO ()
runPartWith pDigit =
    putStrLn
        . either id show
        . parseOnly (pNumbers pDigit)
        . T.pack

part1 :: String -> IO ()
part1 = runPartWith singleDigit

singleDigitSpelled :: Parser Int
singleDigitSpelled = singleDigit <|> spelledDigit
  where
    digitStrings = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    -- spelled digits can overlap by 1 letter so we can't consume all the input
    spelledDigitN n spelling = (lookAhead (string spelling) *> take (T.length spelling - 1)) $> n
    spelledDigit = choice $ zipWith spelledDigitN [1 ..] digitStrings

part2 :: String -> IO ()
part2 = runPartWith singleDigitSpelled
