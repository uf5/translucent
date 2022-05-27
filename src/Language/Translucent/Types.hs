module Language.Translucent.Types where

data LispVal
  = None
  | Bool Bool
  | Int Integer
  | Float Float
  | String String
  | Symbol String
  | Keyword String
  | SExp [LispVal]
  | List [LispVal]
  | Tuple [LispVal]
  | Set [LispVal]
  deriving (Eq, Show)
