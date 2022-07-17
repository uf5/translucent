module Language.Translucent.Exceptions where

import Control.Monad.Except
import Data.Ord

type TransExceptT m = ExceptT TransException m

newtype TransException
  = TransError String
  deriving (Show)
