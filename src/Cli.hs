{-# LANGUAGE OverloadedStrings #-}

module Cli (Part (..), Cli (..), parseCli) where

import qualified Data.Attoparsec.Text as A
import Data.Functor (($>))
import qualified Data.Text as T
import Options.Applicative

data Part = One | Two | Both
    deriving (Show)
data Cli = Cli
    { part :: Part
    , file :: Maybe FilePath
    , day :: Word
    }
    deriving (Show)

partReader :: ReadM Part
partReader = eitherReader (A.parseOnly pPart . T.pack)
  where
    pPart =
        A.choice
            [ A.string "1" $> One
            , A.string "2" $> Two
            , A.string "both" $> Both
            ]

cli :: Parser Cli
cli =
    Cli
        <$> option
            partReader
            ( long "part"
                <> short 'p'
                <> metavar "[1|2|both]"
                <> value Both
                <> help "part of day to run."
            )
        <*> optional
            ( strOption
                ( long "file"
                    <> short 'f'
                    <> metavar "FILE"
                    <> help "input file. defaults to data/day<DAY>.txt."
                )
            )
        <*> argument auto (metavar "DAY" <> help "day to run. should be an int in the range 1..=25.")

parseCli :: IO Cli
parseCli =
    execParser $
        info
            (cli <**> helper)
            ( fullDesc
                <> header "aoc - run solutions to 2023 aoc"
            )
