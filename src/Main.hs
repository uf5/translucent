module Main where

import Parser

main :: IO ()
main = getLine >>= putTextLn . handleEither . parse
  where
    handleEither (Left x) = x
    handleEither (Right x) = show x
