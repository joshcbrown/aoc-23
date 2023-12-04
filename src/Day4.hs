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
import Data.Vector ((!))
import qualified Data.Vector as V

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

nMatches :: Card -> Int
nMatches c = S.length $ S.intersection (winning c) (draw c)

totalScore :: [Card] -> Int
totalScore = sum . map score
  where
    score c = let n = nMatches c in if n == 0 then 0 else 2 ^ (n - 1)

part1 :: String -> IO ()
part1 = runPartWith file totalScore

win :: V.Vector Int -> (Int, Int) -> V.Vector Int
win v (idx, n) =
    let slice = V.slice (idx + 1) n v
        curVal = v ! idx
        newVals = V.map (+ curVal) slice
        range = V.fromListN n [idx + 1 .. idx + n]
        indices = V.zip range newVals
     in V.update v indices

totalScratchCards :: [Card] -> V.Vector Int
totalScratchCards cs =
    let n = length cs
        ps = (zip [0 ..] . map nMatches) cs
     in foldl win (V.replicate n 1) ps

part2 :: String -> IO ()
part2 = runPartWith file (sum . totalScratchCards)
