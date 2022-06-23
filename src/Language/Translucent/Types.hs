module Language.Translucent.Types where

import Data.Text (Text)

data LispVal
  = None
  | Bool Bool
  | Int Integer
  | Float Float
  | String Text
  | Symbol Text
  | Keyword Text
  | SExp [LispVal]
  | List [LispVal]
  | Tuple [LispVal]
  | Set [LispVal]
  deriving (Eq, Show)
