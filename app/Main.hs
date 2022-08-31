module Main where

import AstJson ()
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Language.Translucent
import Language.Translucent.ParserTypes
import System.IO
import Text.Megaparsec

main :: IO ()
main = do
  strInput <- getContents
  case readProgram "stdin" strInput of
    (Left err) -> hPutStrLn stderr (errorBundlePretty err)
    (Right lisp) -> case transModule lisp of
      (Left TransError {location = loc, message = msg}) -> case loc of
        Generated -> hPutStrLn stderr ("error in generated expression: " <> msg)
        Location {offset = off} -> do
          let s =
                PosState
                  { pstateInput = strInput,
                    pstateOffset = 0,
                    pstateSourcePos = initialPos "",
                    pstateTabWidth = defaultTabWidth,
                    pstateLinePrefix = ""
                  }
          let b =
                ParseErrorBundle
                  { bundleErrors = NonEmpty.fromList [FancyError off (Set.fromList [ErrorFail msg])],
                    bundlePosState = s
                  } :: ParserErrorBundle
          hPutStrLn stderr (errorBundlePretty b)
      (Right ast) -> B.putStrLn (encode ast)
