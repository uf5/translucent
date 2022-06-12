module Main where

import Language.Translucent.Expansion
import Language.Translucent.Parser
import Language.Translucent.PythonAst (Statement)
import Language.Translucent.Result (block, (+++))
import Language.Translucent.Trans

main :: IO ()
main = getContents >>= print . readProgram "stdin"
