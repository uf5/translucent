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
  [ ('P.Constant, ["value"]),
    ('P.Name, ["id", "ctx"]),
    ('P.BinOp, ["left", "op", "right"]),
    ('P.Compare, ["left", "ops", "comparators"]),
    ('P.Call, ["func", "args", "keywords"]),
    ('P.List, ["elts", "ctx"]),
    ('P.Tuple, ["elts", "ctx"]),
    ('P.IfExp, ["test", "body", "orelse"])
  ]

instance ToJSON P.Module where
  toJSON (P.Module body type_ignores) = object ["tag" .= String "Module", "body" .= body, "type_ignores" .= type_ignores]

instance ToJSON P.Keyword

instance ToJSON P.Const where
  toJSON P.None = Null
  toJSON (P.Bool v) = toJSON v
  toJSON (P.Int v) = toJSON v
  toJSON (P.Float v) = toJSON v
  toJSON (P.String v) = toJSON v

instance ToJSON P.Statement

instance ToJSON P.ExpressionContext where
  toJSON = dataToTag

instance ToJSON P.Arguments where
  toJSON (P.Arguments posonlyargs args vararg kwonlyargs kw_defaults kwarg defaults) =
    object
      [ "tag" .= String "arguments",
        "posonlyargs" .= posonlyargs,
        "args" .= args,
        "vararg" .= vararg,
        "kwonlyargs" .= kwonlyargs,
        "kw_defaults" .= kw_defaults,
        "kwarg" .= kwarg,
        "defaults" .= defaults
      ]

instance ToJSON P.Arg where
  toJSON (P.Arg arg annotation typeComment) =
    object ["tag" .= String "arg", "arg" .= arg, "annotation" .= annotation, "type_comment" .= typeComment]

instance ToJSON P.BinaryOperator where
  toJSON = dataToTag

instance ToJSON P.CmpOp where
  toJSON = dataToTag

dataToTag :: (Data a) => a -> Value
dataToTag x = object ["tag" .= showConstr (toConstr x)]
