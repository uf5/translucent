module Main where

import Data.Text.IO (hPutStrLn)
import Parser
import Shower

main :: IO ()
main = do
  prog <- getLine
  case parse prog of
    (Left x) -> hPutStrLn stderr x
    (Right x) -> putStrLn (shower x)
