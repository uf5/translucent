module Main where

import Data.Aeson.Micro (encode)
import Data.ByteString.Lazy as B
import Language.Translucent.PythonAst (Module (Module))
import Repl (repl)
import System.Console.Haskeline
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  s <- runInputT defaultSettings (repl [])
  B.writeFile "out.json" $ encode $ Module s []
