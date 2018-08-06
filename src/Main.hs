module Main where

import System.Environment ( getArgs )
import System.Process ( callProcess )
import Parser ( parse2Latex )
import Text.Megaparsec ( parse, parseErrorPretty )
import System.Directory ( removeFile )
import System.IO.Error ( isDoesNotExistError )
import Control.Exception ( catch, throwIO )

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
  removeLatexJunk fileRoot

striptx :: String -> String
striptx filepath =
    case (reverse filepath) of
        'x' : 't' : '.' : htapelif -> reverse htapelif
        _ -> filepath

junkFileExtensions :: [String]
junkFileExtensions =
    ["aux", "bbl", "bcf", "blg", "fdb_latexmk", "fls", "log", "out", "run.xml", "tex"]

junkFiles :: String -> [String]
junkFiles fileRoot = map ((fileRoot ++ ".") ++) junkFileExtensions

removeLatexJunk :: String -> IO()
removeLatexJunk fileRoot = mapM_ removeFileIfThere $ junkFiles fileRoot

removeFileIfThere :: String -> IO()
removeFileIfThere filePath =
    removeFile filePath `catch` handleExists
  where
    handleExists e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e
