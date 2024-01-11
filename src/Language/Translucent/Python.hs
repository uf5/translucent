{-# LANGUAGE DeriveGeneric #-}

module Language.Translucent.Python where

-- See <https://docs.python.org/3/library/ast.html>

import Data.Aeson qualified as J
import Data.Char (toLower)
import GHC.Generics (Generic)

data Module = Module {_body :: [Statement], _type_ignores :: [String]}
  deriving (Show, Generic)

data Keyword = Keyword
  { arg :: String,
    value :: Expression
  }
  deriving (Show, Generic)

data Alias = Alias
  { _name :: String,
    _asname :: Maybe String
  }
  deriving (Show, Generic)

data Constant
  = None
  | Bool Bool
  | Int Integer
  | Float Float
  | String String
  deriving (Show, Generic)

data Statement
  = Expr {_value :: Expression}
  | Return {_value :: Expression}
  | If
      { _test :: Expression,
        _body :: [Statement],
        _orelse :: [Statement]
      }
  | Assign
      { _targets :: [Expression],
        _value :: Expression,
        _type_comment :: Maybe String
      }
  | FunctionDef
      { _name :: String,
        _args :: Arguments,
        _body :: [Statement],
        _decorator_list :: [Expression],
        _returns :: Maybe Expression,
        _type_comment :: Maybe String
      }
  | Import
      { _names :: [Alias]
      }
  | Pass
  deriving (Show, Generic)

data Expression
  = Constant {_value :: Constant}
  | Name {_id :: String, _ctx :: ExpressionContext}
  | NamedExpr {_target :: Expression, value :: Expression}
  | BinOp
      { _left :: Expression,
        _op :: BinaryOperator,
        _right :: Expression
      }
  | Compare
      { _left :: Expression,
        _ops :: [CmpOp],
        _comparators :: [Expression]
      }
  | Call
      { _func :: Expression,
        _args :: [Expression],
        _keywords :: [Keyword]
      }
  | List {_elts :: [Expression], _ctx :: ExpressionContext}
  | Tuple {_elts :: [Expression], _ctx :: ExpressionContext}
  | IfExp {_test :: Expression, _body :: Expression, _orelse :: Expression}
  | Dict {_keys :: [Expression], _values :: [Expression]}
  deriving (Show, Generic)

data Arguments = Arguments {_posonlyargs :: [Arg], _args :: [Arg], _vararg :: Maybe Arg, _kwonlyargs :: [Arg], _kw_defaults :: [Expression], _kwarg :: Maybe Arg, _defaults :: [Expression]}
  deriving (Show, Generic)

data Arg = Arg {_arg :: String, _annotation :: Maybe Expression, _type_comment :: Maybe String}
  deriving (Show, Generic)

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
  deriving (Show, Generic)

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
  deriving (Show, Generic)

data ExpressionContext = Load | Store | Del
  deriving (Show, Generic)

-- JSON

jsonOpts :: J.Options
jsonOpts =
  J.defaultOptions
    { J.tagSingleConstructors = True,
      J.allNullaryToStringTag = False,
      J.fieldLabelModifier = stripUnderscore
    }
  where
    stripUnderscore = dropWhile (== '_')

jsonOptsLower :: J.Options
jsonOptsLower = jsonOpts {J.constructorTagModifier = map toLower}

instance J.ToJSON Constant where
  toJSON None = J.Null
  toJSON (Bool x) = J.toJSON x
  toJSON (Int x) = J.toJSON x
  toJSON (Float x) = J.toJSON x
  toJSON (String x) = J.toJSON x

instance J.ToJSON Expression where
  toEncoding = J.genericToEncoding jsonOpts

instance J.ToJSON Statement where
  toEncoding = J.genericToEncoding jsonOpts

instance J.ToJSON Module where
  toEncoding = J.genericToEncoding jsonOpts

instance J.ToJSON Keyword where
  toEncoding = J.genericToEncoding jsonOptsLower

instance J.ToJSON Alias where
  toEncoding = J.genericToEncoding jsonOptsLower

instance J.ToJSON Arguments where
  toEncoding = J.genericToEncoding jsonOptsLower

instance J.ToJSON Arg where
  toEncoding = J.genericToEncoding jsonOptsLower

instance J.ToJSON ExpressionContext where
  toEncoding = J.genericToEncoding jsonOpts

instance J.ToJSON BinaryOperator where
  toEncoding = J.genericToEncoding jsonOpts

instance J.ToJSON CmpOp where
  toEncoding = J.genericToEncoding jsonOpts
