module Repl (repl) where

import System.Console.Haskeline
import Language.Translucent.PythonAst (Statement)
import Language.Translucent.Parser (readScript)
import Language.Translucent.Trans (trans)
import Language.Translucent.Result ((+++), block)
import Language.Translucent.Expansion (expandModule)

repl :: [Statement] -> InputT IO [Statement]
repl acc = do
  inp <- getInputLine "trans> "
  case inp of
       Nothing -> return acc
       Just "" -> return acc
       Just s -> repl (acc ++ block (foldl1 (+++) $ map trans $ expandModule $ readScript s))
