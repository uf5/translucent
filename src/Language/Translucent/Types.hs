module Language.Translucent.Types where

import Data.Text (Text, unpack)

data Lisp
  = None
  | Bool Bool
  | Int Integer
  | Float Float
  | String Text
  | Symbol Text
  | Keyword Text
  | SExp [Lisp]
  | List [Lisp]
  | Tuple [Lisp]
  | Set [Lisp]

instance Show Lisp where
  show None = "None"
  show (Bool x) = show x
  show (Int x) = show x
  show (Float x) = show x
  show (String x) = show x
  show (Symbol x) = unpack x
  show (Keyword x) = ":" ++ unpack x
  show (SExp x) = "(" ++ unwords (map show x) ++ ")"
  show (List x) = "[" ++ unwords (map show x) ++ "]"
  show (Tuple x) = "#(" ++ unwords (map show x) ++ ")"
  show (Set x) = "{" ++ unwords (map show x) ++ "}"
