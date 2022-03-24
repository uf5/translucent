{-# LANGUAGE OverloadedStrings #-}

module AstJson where

import Data.Aeson
import Data.Data
import GHC.Generics (Generic)
import qualified PythonAST as P

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

instance ToJSON P.Expression

instance ToJSON P.Operator

instance ToJSON P.ExpressionContext where
  toJSON = constToTag

constToTag :: (Data a) => a -> Value
constToTag x = object ["tag" .= showConstr (toConstr x)]
