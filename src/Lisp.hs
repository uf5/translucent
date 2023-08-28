module Lisp (Location, Lisp, Lisp' (..)) where

type Location = Int

type Lisp = (Lisp', Location)

data Lisp'
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
