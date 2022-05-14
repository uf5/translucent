{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AstJson where

import Data.Aeson
import Data.Data
import GHC.Generics (Generic)
import JsonTemplate
import qualified PythonAst as P

jsonFields
  ''P.Expression
  Nothing
  [ ('P.Constant, ["value"]),
    ('P.Name, ["id", "ctx"]),
    ('P.BinOp, ["left", "op", "right"]),
    ('P.Compare, ["left", "ops", "comparators"]),
    ('P.Call, ["func", "args", "keywords"]),
    ('P.List, ["elts", "ctx"]),
    ('P.Tuple, ["elts", "ctx"]),
    ('P.IfExp, ["test", "body", "orelse"])
  ]

jsonFields
  ''P.Statement
  Nothing
  [ ('P.Expr, ["value"]),
    ('P.Return, ["value"]),
    ('P.If, ["test", "body", "orelse"]),
    ('P.FunctionDef, ["name", "args", "body", "decorator_list", "returns", "type_comment"]),
    ('P.Pass, [])
  ]

jsonFields
  ''P.Module
  Nothing
  [('P.Module, ["body", "type_ignores"])]

jsonFields
  ''P.Keyword
  Nothing
  [('P.Keyword, ["args", "value"])]

jsonFields
  ''P.Arguments
  (Just "arguments")
  [('P.Arguments, ["posonlyargs", "args", "vararg", "kwonlyargs", "kw_defaults", "kwarg", "defaults"])]

jsonFields
  ''P.Arg
  (Just "arg")
  [('P.Arg, ["arg", "annotation", "type_comment"])]

instance ToJSON P.Const where
  toJSON P.None = Null
  toJSON (P.Bool v) = toJSON v
  toJSON (P.Int v) = toJSON v
  toJSON (P.Float v) = toJSON v
  toJSON (P.String v) = toJSON v

instance ToJSON P.ExpressionContext where
  toJSON = dataToTag

instance ToJSON P.BinaryOperator where
  toJSON = dataToTag

instance ToJSON P.CmpOp where
  toJSON = dataToTag

dataToTag :: (Data a) => a -> Value
dataToTag x = object ["tag" .= showConstr (toConstr x)]
