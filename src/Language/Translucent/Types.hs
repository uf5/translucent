module Language.Translucent.Types where

import Data.Text (Text)

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
  deriving (Eq, Show)
