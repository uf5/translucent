{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PythonAST where

import qualified Data.Aeson as A
import GHC.Generics

newtype PyModule = PyModule {body :: [Statement]}
  deriving (Eq, Show, Generic)

instance A.ToJSON PyModule

data Keyword = Keyword {args :: String, value :: Expression}
  deriving (Eq, Show, Generic)

instance A.ToJSON Keyword

data Const
  = None
  | Bool Bool
  | Int Integer
  | Float Float
  | String String
  deriving (Eq, Show, Generic)

instance A.ToJSON Const where
  toJSON None = A.Null
  toJSON (Bool v) = A.toJSON v
  toJSON (Int v) = A.toJSON v
  toJSON (Float v) = A.toJSON v
  toJSON (String v) = A.toJSON v

data Statement
  = Expression {value :: Expression}
  | If {test :: Expression, body :: [Statement], orelse :: [Statement]}
  deriving (Eq, Show, Generic)

instance A.ToJSON Statement

data Expression
  = Constant {value :: Const}
  | Name {id :: String, exp_context :: ExpressionContext}
  | BinOp {left :: Expression, op :: Operator, right :: Expression}
  | Call {func :: Expression, args :: [Expression], keywords :: [Keyword]}
  | List {elts :: [Expression], ctx :: ExpressionContext}
  | Tuple {elts :: [Expression], ctx :: ExpressionContext}
  deriving (Eq, Show, Generic)

instance A.ToJSON Expression

data Operator
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
  deriving (Eq, Show, Generic)

instance A.ToJSON Operator

data ExpressionContext = Load | Store | Del
  deriving (Eq, Show, Generic)

instance A.ToJSON ExpressionContext
