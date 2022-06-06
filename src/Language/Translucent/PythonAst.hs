module Language.Translucent.PythonAst where

import Data.Text

data Module = Module [Statement] [Text]
  deriving (Eq, Show)

data Keyword = Keyword Text Expression
  deriving (Eq, Show)

data Const
  = None
  | Bool Bool
  | Int Integer
  | Float Float
  | String Text
  deriving (Eq, Show)

data Statement
  = Expr Expression
  | Return Expression
  | If Expression [Statement] [Statement]
  | FunctionDef Text Arguments [Statement] [Expression] (Maybe Expression) (Maybe Text)
  | Pass
  deriving (Eq, Show)

data Expression
  = Constant Const
  | Name Text ExpressionContext
  | BinOp Expression BinaryOperator Expression
  | Compare Expression [CmpOp] [Expression]
  | Call Expression [Expression] [Keyword]
  | List [Expression] ExpressionContext
  | Tuple [Expression] ExpressionContext
  | IfExp Expression Expression Expression
  deriving (Eq, Show)

data Arguments = Arguments [Arg] [Arg] (Maybe Arg) [Arg] [Expression] (Maybe Arg) [Expression]
  deriving (Eq, Show)

data Arg = Arg Text (Maybe Expression) (Maybe Text)
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data ExpressionContext = Load | Store | Del
  deriving (Eq, Show)
