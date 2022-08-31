{-# LANGUAGE FlexibleContexts #-}

module Language.Translucent.Exceptions where

import Control.Monad.Except
import Language.Translucent.Lisp
import Text.Megaparsec.Pos

type TransExceptT m = ExceptT TransError m

data TransError = TransError
  { location :: Location,
    message :: String
  }

throwTransError loc msg = throwError $ TransError loc msg
