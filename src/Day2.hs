{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day2 (
    part1,
    part2,
) where

import Control.Applicative
import Control.Monad (void)
import Data.Attoparsec.Text
import qualified Data.Text as T

data Balls = Blue Int | Red Int | Green Int
    deriving (Show)
type Round = [Balls]
data Bag = Bag {red :: Int, blue :: Int, green :: Int}
    deriving (Show)
data Game = Game {game_id :: Int, rounds :: [Round]}
    deriving (Show)

balls :: T.Text -> Parser Int
balls t = decimal <* string t

pRound :: Parser Round
pRound =
    many $
        choice
            [ Blue <$> balls " blue"
            , Red <$> balls " red"
            , Green <$> balls " green"
            ]
            <* optional (string ", ")

pRounds :: Parser Game
pRounds = Game <$> gid <*> game_rounds
  where
    gid = string "Game " *> decimal <* string ": "
    game_rounds = many $ pRound <* (void (string "; ") <|> endOfLine)

pPenis :: Parser [Game]
pPenis = many pRounds <* endOfInput

validate :: Game -> Bool
validate = all (all ok) . rounds
  where
    ok (Red n) = n <= 12
    ok (Green n) = n <= 13
    ok (Blue n) = n <= 14

runPartWith :: ([Game] -> String) -> String -> IO ()
runPartWith f =
    putStrLn
        . either id f
        . parseOnly pPenis
        . T.pack

part1 :: String -> IO ()
part1 = runPartWith (show . sum . map game_id . filter validate)

foldFunc :: Bag -> Balls -> Bag
foldFunc acc (Blue n) = acc{blue = max n (blue acc)}
foldFunc acc (Red n) = acc{red = max n (red acc)}
foldFunc acc (Green n) = acc{green = max n (green acc)}

power :: Bag -> Int
power b = red b * blue b * green b

part2 :: String -> IO ()
part2 = runPartWith (show . sum . map (power . foldl foldFunc Bag{red = 0, green = 0, blue = 0} . concat . rounds))
