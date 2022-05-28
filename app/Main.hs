module Main where

import Data.Aeson.Micro (encode)
import qualified Data.ByteString.Lazy as B
import Language.Translucent.Expansion (expandModule)
import Language.Translucent.Parser (readScript)
import Language.Translucent.PythonAst (Module (Module), Statement)
import Language.Translucent.Result (block, (+++))
import Language.Translucent.Trans (trans)
import System.Console.Haskeline
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  m <- progInput
  B.putStr $ encode m

progInput :: IO Module
progInput = do
  s <- runInputT defaultSettings (loop [])
  let prog = block $ foldl1 (+++) $ map trans $ expandModule $ readScript s
  return $ Module prog []
  where
    loop :: String -> InputT IO String
    loop acc = do
      inp <- getInputLine ""
      case inp of
        Nothing -> return acc
        Just "" -> return acc
        Just s -> loop $ acc ++ s
