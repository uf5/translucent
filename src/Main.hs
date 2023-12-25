module Main where

import Language.Translucent

main :: IO ()
main = getLine >>= print . parseProgram
