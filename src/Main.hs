module Main where

import Codegen
import Data.Text.IO (hPutStrLn)
import Optics
import Parser
import Shower

main :: IO ()
main = do
  putStr "translucent> "
  hFlush stdout
  prog <- getLine
  case parse prog of
    (Left x) -> hPutStrLn stderr x
    (Right x) -> do
      putStrLn (shower x)
      let translated = execState (runCodegen (cgen x)) initialState
      let resultBlock = translated ^. block
      putStrLn ("statements: " <> shower (resultBlock ^. stmts))
      putStrLn ("final expr: " <> shower (resultBlock ^. expr))
