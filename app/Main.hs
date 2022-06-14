module Main where

import AstJson
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as B
import Language.Translucent.Parser
import Language.Translucent.Result (block, (+++))
import Language.Translucent.Trans

main :: IO ()
main = getContents >>= B.putStrLn . encode . transModule . unwrap_either . readProgram "stdin"
  where
    unwrap_either :: Show a => Either a b -> b
    unwrap_either (Left x) = error (show x)
    unwrap_either (Right x) = x
