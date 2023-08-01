module Lisp (Lisp (..)) where

data Lisp
  = None
  | Int Integer
  | Float Float
  | String Text
  | Symbol Text
  | Keyword Text
  | SExp [Lisp]
  | List [Lisp]
  | Dict [Lisp]
  deriving (Show)
