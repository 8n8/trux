module Main where

import System.Environment ( getArgs )
import System.Process ( callProcess )
import Parser ( parseDocument )
import Text.Megaparsec ( parse, sourcePosStackPretty )

main :: IO ()
main = do
  filepath <- fmap head getArgs
  filecontents <- readFile filepath
  let texfile = (striptx filepath) ++ ".tex"
  case parse parseDocument filepath filecontents of
      Left err -> putStrLn (sourcePosStackPretty err)
      Right latex -> do
          writeFile texfile latex
          _ <- callProcess "latexmk" ["-pdf", "-interaction=nonstopmode", texfile]
          return ()

striptx :: String -> String
striptx filepath =
    case (reverse filepath) of
        'x' : 't' : '.' : htapelif -> reverse htapelif
        _ -> filepath

