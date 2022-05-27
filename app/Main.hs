module Main where

import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy.IO as I
import System.Environment (getArgs)
import System.IO

import Language.Translucent.Parser
import Language.Translucent.Expansion
import Language.Translucent.Trans
import Language.Translucent.AstJson

import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "% "
           case minput of
               Nothing -> return ()
               Just "" -> return ()
               Just s -> do
                 let parsed = readScript s
                 outputStrLn $ show parsed
                 let expanded = expandModule parsed
                 outputStrLn $ show expanded
                 let translated = transModule expanded
                 outputStrLn $ show translated
                 liftIO $ I.writeFile "out.json" (encodeToLazyText translated)
                 loop
