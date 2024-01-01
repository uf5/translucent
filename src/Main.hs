module Main where

import Language.Translucent
import Language.Translucent.Python qualified as P

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

compile :: String -> Either GeneralError [P.Statement]
compile prog = do
  parsed <- mapLeft pe2ge (parseProgram prog)
  mapLeft te2ge (transpile parsed)

main :: IO ()
main = do
  prog <- getContents
  case compile prog of
    Left err -> putStrLn (displayErrorInCode prog err)
    Right x -> print x
