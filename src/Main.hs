module Main where

import System.Environment ( getArgs )
import System.Process ( callProcess )
import Parser ( parse2Latex )
import Text.Megaparsec ( parse, errorBundlePretty )
import System.Directory ( removeFile )
import System.IO.Error ( isDoesNotExistError )
import Control.Exception ( catch, throwIO )
import qualified System.FSNotify as Fsn
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

main :: IO ()
main = do
  filepath <- fmap head getArgs
  runOnce filepath
  let container = containingDir filepath
  _ <- Fsn.withManager $ \mgr -> do
    _ <- Fsn.watchDir
        mgr
        container
        (predicate filepath)
        (\_ -> runOnce filepath)
    forever $ threadDelay 1000000
  return ()

containingDir :: String -> String
containingDir filePath =
    let
        f :: String -> String
        f "" = "."
        f ('/' : rs) = rs
        f (_:rs) = f rs
    in
        reverse . f . reverse $ filePath

fileName :: String -> String
fileName filePath =
    let
        f :: String -> String -> String
        f "" filename = reverse filename
        f ('/':_) filename = filename
        f (c:cs) filename = f cs (c:filename)
    in
        f (reverse filePath) ""

predicate :: String -> Fsn.Event -> Bool
predicate filePath (Fsn.Modified fpModified _ _) =
    (fileName fpModified) == filePath
predicate _ _ = False

runOnce :: String -> IO ()
runOnce filepath = do
  filecontents <- readFile filepath
  let fileRoot = (striptx filepath)
  case parse parse2Latex filepath filecontents of
      Left err -> putStrLn (errorBundlePretty err)
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
    ["aux", "bbl", "bcf", "blg", "fdb_latexmk", "fls", "log", "out", "run.xml", "tex", "toc"]

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
