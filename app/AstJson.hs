{-# OPTIONS_GHC -Wno-orphans #-}

module AstJson where

import Data.Aeson
  ( Options (..),
    ToJSON,
    Value (Null),
    defaultOptions,
    genericToEncoding,
    toEncoding,
    toJSON,
  )
import Data.Char (toLower)
import Language.Translucent.PythonAst

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { tagSingleConstructors = True,
      allNullaryToStringTag = False,
      fieldLabelModifier = stripFn
    }
  where
    stripFn ('_' : x) = x
    stripFn x = x

jsonOptionsLowerConstructor :: Options
jsonOptionsLowerConstructor =
  jsonOptions
    { constructorTagModifier = map toLower
    }

instance ToJSON Const where
  toJSON None = Null
  toJSON (Bool v) = toJSON v
  toJSON (Int v) = toJSON v
  toJSON (Float v) = toJSON v
  toJSON (String v) = toJSON v

instance ToJSON Expression where
  toEncoding = genericToEncoding jsonOptions

instance ToJSON Statement where
  toEncoding = genericToEncoding jsonOptions

instance ToJSON Module where
  toEncoding = genericToEncoding jsonOptions

instance ToJSON Keyword where
  toEncoding = genericToEncoding jsonOptions

instance ToJSON Arguments where
  toEncoding = genericToEncoding jsonOptionsLowerConstructor

instance ToJSON Arg where
  toEncoding = genericToEncoding jsonOptionsLowerConstructor

instance ToJSON ExpressionContext where
  toEncoding = genericToEncoding jsonOptions

instance ToJSON BinaryOperator where
  toEncoding = genericToEncoding jsonOptions

instance ToJSON CmpOp where
  toEncoding = genericToEncoding jsonOptions
