module Language.Translucent.ParserTypes
  ( ParserCustomError,
    ParserInputStream,
    Parser,
    ParserErrorBundle,
  )
where

import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec)

type ParserCustomError = Void

type ParserInputStream = String

type Parser = Parsec ParserCustomError ParserInputStream

type ParserErrorBundle = ParseErrorBundle ParserInputStream ParserCustomError
