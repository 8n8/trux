module Main where

import System.Environment ( getArgs )
import System.Process ( callProcess )
import Parser ( parse2Latex )
import Text.Megaparsec ( parse, parseErrorPretty )

main :: IO ()
main = do
  filepath <- fmap head getArgs
  filecontents <- readFile filepath
  let fileRoot = (striptx filepath)
  case parse parse2Latex filepath filecontents of
      Left err -> putStrLn (parseErrorPretty err)
      Right latex -> do
          writeFile (fileRoot ++ ".tex") latex
          _ <- callProcess "latexmk"
              ["-pdf", "-interaction=nonstopmode", fileRoot ++ ".tex"]
          return ()

striptx :: String -> String
striptx filepath =
    case (reverse filepath) of
        'x' : 't' : '.' : htapelif -> reverse htapelif
        _ -> filepath
