{-# LANGUAGE DeriveGeneric #-}

module Language.Translucent.PythonAst where

import GHC.Generics (Generic)

data Module = Module [Statement] [String]
  deriving (Eq, Show, Generic)

data Keyword = Keyword String Expression
  deriving (Eq, Show, Generic)

data Const
  = None
  | Bool Bool
  | Int Integer
  | Float Float
  | String String
  deriving (Eq, Show, Generic)

data Statement
  = Expr Expression
  | Return Expression
  | If Expression [Statement] [Statement]
  | FunctionDef String Arguments [Statement] [Expression] (Maybe Expression) (Maybe String)
  | Pass
  deriving (Eq, Show, Generic)

data Expression
  = Constant Const
  | Name String ExpressionContext
  | BinOp Expression BinaryOperator Expression
  | Compare Expression [CmpOp] [Expression]
  | Call Expression [Expression] [Keyword]
  | List [Expression] ExpressionContext
  | Tuple [Expression] ExpressionContext
  | IfExp Expression Expression Expression
  deriving (Eq, Show, Generic)

data Arguments = Arguments [Arg] [Arg] (Maybe Arg) [Arg] [Expression] (Maybe Arg) [Expression]
  deriving (Eq, Show, Generic)

data Arg = Arg String (Maybe Expression) (Maybe String)
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
  deriving (Eq, Show, Generic)

data CmpOp
  = Eq
  | NotEq
  | Lt
  | LtE
  | Gt
  | GtE
  | Is
  | IsNot
  | In
  | NotIn
  deriving (Eq, Show, Generic)

data ExpressionContext = Load | Store | Del
  deriving (Eq, Show, Generic)
