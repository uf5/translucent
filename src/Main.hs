module Main where

import Data.Text.IO (hPutStrLn)
import Parser

main :: IO ()
main = do
  prog <- getLine
  case parse prog of
    (Left x) -> hPutStrLn stderr x
    (Right x) -> putTextLn (show x)
