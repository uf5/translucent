{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Translucent.PythonAst where

import Data.Aeson.Micro (ToJSON, Value (Null), toJSON)
import Data.Text
import GHC.Generics (Generic)
import Language.Translucent.JsonTemplate

data Module = Module [Statement] [Text]
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
  = Expr Expression
  | Return Expression
  | If Expression [Statement] [Statement]
  | FunctionDef Text Arguments [Statement] [Expression] (Maybe Expression) (Maybe Text)
  | Pass
  deriving (Eq, Show, Generic)

data Expression
  = Constant Const
  | Name Text ExpressionContext
  | BinOp Expression BinaryOperator Expression
  | Compare Expression [CmpOp] [Expression]
  | Call Expression [Expression] [Keyword]
  | List [Expression] ExpressionContext
  | Tuple [Expression] ExpressionContext
  | IfExp Expression Expression Expression
  deriving (Eq, Show, Generic)

data Arguments = Arguments [Arg] [Arg] (Maybe Arg) [Arg] [Expression] (Maybe Arg) [Expression]
  deriving (Eq, Show, Generic)

data Arg = Arg Text (Maybe Expression) (Maybe Text)
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

jsonFields
  ''Expression
  Nothing
  [ ('Constant, ["value"]),
    ('Name, ["id", "ctx"]),
    ('BinOp, ["left", "op", "right"]),
    ('Compare, ["left", "ops", "comparators"]),
    ('Call, ["func", "args", "keywords"]),
    ('List, ["elts", "ctx"]),
    ('Tuple, ["elts", "ctx"]),
    ('IfExp, ["test", "body", "orelse"])
  ]

jsonFields
  ''Statement
  Nothing
  [ ('Expr, ["value"]),
    ('Return, ["value"]),
    ('If, ["test", "body", "orelse"]),
    ('FunctionDef, ["name", "args", "body", "decorator_list", "returns", "type_comment"]),
    ('Pass, [])
  ]

jsonFields
  ''Module
  Nothing
  [('Module, ["body", "type_ignores"])]

jsonFields
  ''Keyword
  Nothing
  [('Keyword, ["args", "value"])]

jsonFields
  ''Arguments
  (Just "arguments")
  [('Arguments, ["posonlyargs", "args", "vararg", "kwonlyargs", "kw_defaults", "kwarg", "defaults"])]

jsonFields
  ''Arg
  (Just "arg")
  [('Arg, ["arg", "annotation", "type_comment"])]

jsonDataTag ''ExpressionContext

jsonDataTag ''BinaryOperator

jsonDataTag ''CmpOp

instance ToJSON Const where
  toJSON None = Null
  toJSON (Bool v) = toJSON v
  toJSON (Int v) = toJSON v
  toJSON (Float v) = toJSON v
  toJSON (String v) = toJSON v
