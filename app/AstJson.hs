module AstJson where

import Data.Aeson hiding (Bool, String)
import Data.Char (toLower)
import Language.Translucent.PythonAst

lowerOptions = defaultOptions {constructorTagModifier = map toLower}

tagSingleOptions = defaultOptions {tagSingleConstructors = True}

tagNullaryOptions = defaultOptions {allNullaryToStringTag = False}

instance ToJSON Const where
  toJSON None = Null
  toJSON (Bool v) = toJSON v
  toJSON (Int v) = toJSON v
  toJSON (Float v) = toJSON v
  toJSON (String v) = toJSON v

instance ToJSON Expression where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Statement where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Module where
  toEncoding = genericToEncoding tagSingleOptions

instance ToJSON Keyword where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Arguments where
  toEncoding = genericToEncoding lowerOptions

instance ToJSON Arg where
  toEncoding = genericToEncoding lowerOptions

instance ToJSON ExpressionContext where
  toEncoding = genericToEncoding tagNullaryOptions

instance ToJSON BinaryOperator where
  toEncoding = genericToEncoding tagNullaryOptions

instance ToJSON CmpOp where
  toEncoding = genericToEncoding tagNullaryOptions
