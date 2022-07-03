{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Translucent.PythonAst where

import Data.Text
import GHC.Generics (Generic)

-- See <https://docs.python.org/3/library/ast.html> for more information

data Module = Module {body :: [Statement], type_ignores :: [Text]}
  deriving (Eq, Show, Generic)

data Keyword = Keyword Text Expression
  deriving (Eq, Show, Generic)

data Const
  = None
  | Bool Bool
  | Int Integer
  | Float Float
  | String Text
  deriving (Eq, Show, Generic)

data Statement
  = Expr {value :: Expression}
  | Return {value :: Expression}
  | If
      { test :: Expression,
        body :: [Statement],
        orelse :: [Statement]
      }
  | Assign
      { targets :: [Expression],
        value :: Expression,
        type_comment :: Maybe Text
      }
  | FunctionDef
      { name :: Text,
        args :: Arguments,
        body :: [Statement],
        decorator_list :: [Expression],
        returns :: Maybe Expression,
        type_comment :: Maybe Text
      }
  | Pass
  deriving (Eq, Show, Generic)

data Expression
  = Constant {value :: Const}
  | Name {id :: Text, ctx :: ExpressionContext}
  | BinOp
      { left :: Expression,
        op :: BinaryOperator,
        right :: Expression
      }
  | Compare
      { left :: Expression,
        ops :: [CmpOp],
        comparators :: [Expression]
      }
  | Call
      { func :: Expression,
        args :: [Expression],
        keywords :: [Keyword]
      }
  | List {elts :: [Expression], ctx :: ExpressionContext}
  | Tuple {elts :: [Expression], ctx :: ExpressionContext}
  | IfExp {test :: Expression, body :: Expression, orelse :: Expression}
  deriving (Eq, Show, Generic)

data Arguments = Arguments {posonlyargs :: [Arg], args :: [Arg], vararg :: Maybe Arg, kwonlyargs :: [Arg], kw_defaults :: [Expression], kwarg :: Maybe Arg, defaults :: [Expression]}
  deriving (Eq, Show, Generic)

data Arg = Arg {arg :: Text, annotation :: Maybe Expression, type_comment :: Maybe Text}
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
