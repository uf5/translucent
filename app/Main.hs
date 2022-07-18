{-# LANGUAGE LambdaCase #-}

module Main where

import AstJson ()
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as B
import Language.Translucent

-- TODO: ugly

main :: IO ()
main =
  getContents
    >>= ( \case
            (Left e) -> print e
            (Right x) -> B.putStrLn $ encode x
        )
      . transModule
      . unwrap_either
      . readProgram "stdin"
  where
    unwrap_either :: Show a => Either a b -> b
    unwrap_either (Left x) = error (show x)
    unwrap_either (Right x) = x
