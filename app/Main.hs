module Main where

import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy.IO as I
import Lib
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let repl = do
        putStr "trans> "
        hFlush stdout
        s <- getLine
        case s of
          "" -> return ()
          _ -> do
            let parsed = readScript s
            print parsed
            let translated = transModule parsed
            print translated
            I.writeFile "out.json" (encodeToLazyText translated)
            repl
  repl
