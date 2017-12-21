module Main where

import 
import Text.Megaparsec
-- import Text.Megaparsec.String
import System.Environment
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

main :: IO ()
main = do
    args <- getArgs
    parseTest twoLetterParser $ head args

twoLetterParser :: Parser Char
twoLetterParser = char 'h' >> char 'a'
