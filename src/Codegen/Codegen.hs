{-# LANGUAGE TemplateHaskell #-}

module Codegen.Codegen where

import Optics
import PythonAst qualified as P

data SourceBlock = SourceBlock {_stmts :: [P.Statement], _expr :: P.Expression}

makeLenses ''SourceBlock

data BlockState = BlockState
  { _block :: SourceBlock
  , _names :: Map Text Text
  }

makeLenses ''BlockState

newtype Codegen a = Codegen {runCodegen :: State BlockState a}
  deriving (Functor, Applicative, Monad, MonadState BlockState)

initialState :: BlockState
initialState =
  BlockState
    { _block =
        ( SourceBlock
            { _stmts = []
            , _expr = P.Constant P.None
            }
        )
    , _names = fromList []
    }
