{-# LANGUAGE TemplateHaskell #-}

module AstJson where

import Data.Aeson (ToJSON, Value (Null), toJSON)
import JsonTemplate
import Language.Translucent.PythonAst

-- jsonFields
--   ''Expression
--   Nothing
--   [ ('Constant, ["value"]),
--     ('Name, ["id", "ctx"]),
--     ('BinOp, ["left", "op", "right"]),
--     ('Compare, ["left", "ops", "comparators"]),
--     ('Call, ["func", "args", "keywords"]),
--     ('List, ["elts", "ctx"]),
--     ('Tuple, ["elts", "ctx"]),
--     ('IfExp, ["test", "body", "orelse"])
--   ]
--
-- jsonFields
--   ''Statement
--   Nothing
--   [ ('Expr, ["value"]),
--     ('Return, ["value"]),
--     ('If, ["test", "body", "orelse"]),
--     ('FunctionDef, ["name", "args", "body", "decorator_list", "returns", "type_comment"]),
--     ('Pass, [])
--   ]
--
-- jsonFields
--   ''Module
--   Nothing
--   [('Module, ["body", "type_ignores"])]
--
-- jsonFields
--   ''Keyword
--   Nothing
--   [('Keyword, ["args", "value"])]
--
-- jsonFields
--   ''Arguments
--   (Just "arguments")
--   [('Arguments, ["posonlyargs", "args", "vararg", "kwonlyargs", "kw_defaults", "kwarg", "defaults"])]
--
-- jsonFields
--   ''Arg
--   (Just "arg")
--   [('Arg, ["arg", "annotation", "type_comment"])]
--
-- jsonDataTag ''ExpressionContext
--
-- jsonDataTag ''BinaryOperator
--
-- jsonDataTag ''CmpOp
--
-- instance ToJSON Const where
--   toJSON None = Null
--   toJSON (Bool v) = toJSON v
--   toJSON (Int v) = toJSON v
--   toJSON (Float v) = toJSON v
--   toJSON (String v) = toJSON v
