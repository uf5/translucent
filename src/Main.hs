module Main where

import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy.IO as I
import Parser
import System.Environment (getArgs)
import System.IO
import Trans

main :: IO ()
main = do
  args <- getArgs
  let repl = do
        putStr "trans>"
        hFlush stdout
        s <- getLine
        case s of
          "" -> do
            return ()
          _ -> do
            let parsed = readScript s
            print parsed
            let translated = map transStmt parsed
            print translated
            I.writeFile "out.json" (encodeToLazyText translated)
            repl
  repl
