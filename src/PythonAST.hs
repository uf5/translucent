{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PythonAST where

import Data.Data (Data)
import GHC.Generics (Generic)

data Module = Module {body :: [Statement], type_ignores :: [String]}
  deriving (Eq, Show, Generic)

data Keyword = Keyword {args :: String, value :: Expression}
  deriving (Eq, Show, Generic)

data Const
  = None
  | Bool Bool
  | Int Integer
  | Float Float
  | String String
  deriving (Eq, Show, Generic)

data Statement
  = Expr {value :: Expression}
  | If {test :: Expression, body :: [Statement], orelse :: [Statement]}
  deriving (Eq, Show, Generic)

data Expression
  = Constant {value :: Const}
  | Name {id :: String, ctx :: ExpressionContext}
  | BinOp {left :: Expression, op :: BinaryOperator, right :: Expression}
  | Call {func :: Expression, args :: [Expression], keywords :: [Keyword]}
  | List {elts :: [Expression], ctx :: ExpressionContext}
  | Tuple {elts :: [Expression], ctx :: ExpressionContext}
  | IfExp {test :: Expression, body :: Expression, orelse :: Expression}
  deriving (Eq, Show, Generic)

data BinaryOperator
  = Add
  | Sub
  | Mult
  | MatMult
  | Div
  | Mod
  | Pow
  | LShift
  | RShift
  | BitOr
  | BitXor
  | BitAnd
  | FloorDiv
  deriving (Eq, Show, Generic, Data)

data ExpressionContext = Load | Store | Del
  deriving (Eq, Show, Generic, Data)
