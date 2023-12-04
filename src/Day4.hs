module Day4 (
    part1,
    part2,
) where

import Common (runPartWith)
import Control.Applicative
import Control.Monad (void)
import Data.Attoparsec.Text
import qualified Data.Foldable as S
import qualified Data.Set as S

data Card = Card {card_id :: Int, winning :: S.Set Int, draw :: S.Set Int}
    deriving (Show)

intsEndedBy :: Parser () -> Parser (S.Set Int)
intsEndedBy end = S.fromList <$> (many (many (char ' ') *> decimal) <* end)

card :: Parser Card
card =
    Card
        <$> (string "Card" *> many space *> decimal <* char ':')
        <*> intsEndedBy (void $ string " |")
        <*> intsEndedBy endOfLine

file :: Parser [Card]
file = many card <* endOfInput

totalScore :: [Card] -> Int
totalScore = sum . map score
  where
    nMatches c = S.length $ S.intersection (winning c) (draw c)
    score c = let n = nMatches c in if n == 0 then 0 else 2 ^ (n - 1)

part1 :: String -> IO ()
part1 = runPartWith file totalScore

part2 :: String -> IO ()
part2 = undefined
