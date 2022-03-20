{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}

module PythonAST where

import qualified Data.Aeson as A
import GHC.Generics

type PyModule = [Stmt]

data Keyword =
  Keyword { args :: String, value :: Expr }
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

data Stmt
  = Expression Expr
  | If { test :: Expr, body :: [Stmt], orelse :: [Stmt] }
  deriving (Eq, Show, Generic)

data Expr
  = Constant { value :: Const}
  | Name { id :: String, exp_context :: ExprContext }
  | BinOp { left :: Expr, op :: Operator, right :: Expr }
  | Call { func :: Expr, args :: [Expr], keywords :: [Keyword] }
  | List { elts :: [Expr], ctx :: ExprContext }
  | Tuple { elts :: [Expr], ctx :: ExprContext }
  deriving (Eq, Show, Generic)

instance A.ToJSON Expr

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

data ExprContext = Load | Store | Del
  deriving (Eq, Show, Generic)

instance A.ToJSON ExprContext
