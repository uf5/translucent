module Language.Translucent.Exceptions where

import Control.Monad.Except
import Text.Parsec (ParseError)

type TransExceptT m = ExceptT TransException m

data TransException
  = TransError String
  deriving (Show)
