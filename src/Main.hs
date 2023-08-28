module Main where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as B
import Parser
import Python qualified as P
import Shower
import System.IO (hPutStrLn)
import Text.Megaparsec (errorBundlePretty)
import Transpiler

main :: IO ()
main = do
  putStr "translucent> "
  hFlush stdout
  prog <- getLine
  case parse prog of
    (Left x) -> hPutStrLn stderr (errorBundlePretty x)
    (Right x) -> do
      putStrLn $ shower x
      case runIdentity
        ( runExceptT
            ( evalStateT
                (runTranspiler (trans x *> extract))
                (initialState (TranspilerConfig "__tr_gen_"))
            )
        ) of
        (Left e) -> hPutStrLn stderr ("error " <> show e)
        (Right x') -> B.putStrLn (encode (P.Module [P.Expr (snd x')] []))
