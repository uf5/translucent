{-# LANGUAGE TemplateHaskell #-}

module Transpiler.Monad where

import Control.Monad.Except
import Lisp (Location)
import Optics
import Python

newtype TranspilerConfig = TranspilerConfig
  { _manglerPrefix :: Text
  }

makeLenses ''TranspilerConfig

data Block = Block
  { _stmts :: [Statement]
  , _expr :: Expression
  }

makeLenses ''Block

newtype ManglerState = ManglerState
  { _definedNames :: Map Text Text
  }

makeLenses ''ManglerState

data TranspilerState = TranspilerState
  { _config :: TranspilerConfig
  , _block :: Block
  , _manglerState :: ManglerState
  }

makeLenses ''TranspilerState

data TranspilerError = TE Text Int
  deriving (Show)

newtype Transpiler a = Transpiler
  { runTranspiler :: (StateT TranspilerState (Except TranspilerError)) a
  }
  deriving (Functor, Applicative, Monad, MonadState TranspilerState, MonadError TranspilerError)

initialState :: TranspilerConfig -> TranspilerState
initialState c =
  TranspilerState
    { _config = c
    , _block = initialBlock
    , _manglerState = initialManglerState
    }
  where
    initialBlock =
      Block
        { _stmts = []
        , _expr = Constant None
        }
    initialManglerState =
      ManglerState
        { _definedNames = fromList []
        }
