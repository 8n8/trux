module Main where

import System.Environment ( getArgs )
import Parser ( parseDocument )
import Text.Megaparsec ( parse, parseErrorPretty )

main :: IO ()
main = do
  filepath <- fmap head getArgs
  filecontents <- readFile filepath
  case parse parseDocument filepath filecontents of
      Left err -> putStrLn (parseErrorPretty err)
      Right latex -> writeFile ((striptx filepath) ++ ".tex") latex

striptx :: String -> String
striptx filepath =
    case (reverse filepath) of
        'x' : 't' : '.' : htapelif -> reverse htapelif
        _ -> filepath

