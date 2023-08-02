{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PythonAst where

import Data.Text
import Prelude hiding (Const)

-- See <https://docs.python.org/3/library/ast.html> for more information

data Module = Module {_body :: [Statement], _type_ignores :: [Text]}
  deriving (Eq, Show, Generic)

data Keyword = Keyword
  { arg :: Text
  , value :: Expression
  }
  deriving (Eq, Show, Generic)

data Alias = Alias
  { _name :: Text
  , _asname :: Maybe Text
  }
  deriving (Eq, Show, Generic)

data Const
  = None
  | Bool Bool
  | Int Integer
  | Float Float
  | String Text
  deriving (Eq, Show, Generic)

data Statement
  = Expr {_value :: Expression}
  | Return {_value :: Expression}
  | If
      { _test :: Expression
      , _body :: [Statement]
      , _orelse :: [Statement]
      }
  | Assign
      { _targets :: [Expression]
      , _value :: Expression
      , _type_comment :: Maybe Text
      }
  | FunctionDef
      { _name :: Text
      , _args :: Arguments
      , _body :: [Statement]
      , _decorator_list :: [Expression]
      , _returns :: Maybe Expression
      , _type_comment :: Maybe Text
      }
  | Import
      { _names :: [Alias]
      }
  | Pass
  deriving (Eq, Show, Generic)

data Expression
  = Constant {_value :: Const}
  | Name {_id :: Text, _ctx :: ExpressionContext}
  | NamedExpr {_target :: Expression, value :: Expression}
  | BinOp
      { _left :: Expression
      , _op :: BinaryOperator
      , _right :: Expression
      }
  | Compare
      { _left :: Expression
      , _ops :: [CmpOp]
      , _comparators :: [Expression]
      }
  | Call
      { _func :: Expression
      , _args :: [Expression]
      , _keywords :: [Keyword]
      }
  | List {_elts :: [Expression], _ctx :: ExpressionContext}
  | Tuple {_elts :: [Expression], _ctx :: ExpressionContext}
  | IfExp {_test :: Expression, _body :: Expression, _orelse :: Expression}
  | Dict {_keys :: [Expression], _values :: [Expression]}
  deriving (Eq, Show, Generic)

data Arguments = Arguments {_posonlyargs :: [Arg], _args :: [Arg], _vararg :: Maybe Arg, _kwonlyargs :: [Arg], _kw_defaults :: [Expression], _kwarg :: Maybe Arg, _defaults :: [Expression]}
  deriving (Eq, Show, Generic)

data Arg = Arg {_arg :: Text, _annotation :: Maybe Expression, _type_comment :: Maybe Text}
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
