module Common where

import Data.Attoparsec.Text
import qualified Data.Text as T

runPartWith :: (Show b) => Parser a -> (a -> b) -> String -> IO ()
runPartWith p f =
    putStrLn
        . either id (show . f)
        . parseOnly p
        . T.pack
