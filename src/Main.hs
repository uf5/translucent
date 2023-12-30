module Main where

import Language.Translucent
import Language.Translucent.Python qualified as P

compile :: String -> Either GeneralError [P.Statement]
compile prog = do
  parsed <- mapLeft (pe2ge (length prog)) (parseProgram prog)
  mapLeft (te2ge (length prog)) (transpile parsed)
  where
    mapLeft f = either (Left . f) Right

main :: IO ()
main = getContents >>= print . compile
