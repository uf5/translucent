module Main where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as B
import Language.Translucent
import Language.Translucent.Python qualified as P
import System.IO (hPutStrLn, stderr)

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
    Left err -> hPutStrLn stderr (displayErrorInCode prog err)
    Right x -> B.putStrLn (encode (P.Module x []))
