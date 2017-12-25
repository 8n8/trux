module Main where

import System.Environment
import qualified Parser as P
import Text.Megaparsec

main :: IO ()
main = firstArgument >>= testParser

firstArgument :: IO String
firstArgument = fmap head getArgs

testParser :: String -> IO ()
testParser = parseTest P.parseDocument
