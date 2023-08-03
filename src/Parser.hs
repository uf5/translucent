{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$" #-}
module Parser (Parser.parse) where

import Lisp qualified as T
import Text.Megaparsec as M
import Text.Megaparsec.Char as M
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

pInt :: Parser T.Lisp
pInt = T.Int <$> L.signed empty L.decimal

pFloat :: Parser T.Lisp
pFloat = T.Float <$> L.signed empty L.float

pSymbol :: Parser T.Lisp
pSymbol = T.Symbol . toText <$> M.some letterChar

pKeyword :: Parser T.Lisp
pKeyword = T.Keyword . toText <$> (char ':' *> M.some letterChar)

pString :: Parser T.Lisp
pString = T.String . toText <$> between (char '"') (char '"') (M.many stringElem)
  where
    stringElem = escapeSeq <|> anySingleBut '"'
    escapeSeq = char '\\' *> choice (map (\(from, to) -> pure to <$> char from) escapeChars)
    escapeChars =
      [ ('\\', '\\')
      , ('\"', '\"')
      , ('a', '\a')
      , ('b', '\b')
      , ('f', '\f')
      , ('n', '\n')
      , ('r', '\r')
      , ('t', '\t')
      , ('v', '\v')
      ]

pLists :: Parser T.Lisp
pLists = choice (map genParser listOfParens)
  where
    genParser (cOpen, cClose, constructor) = constructor <$> between (char cOpen) (char cClose) (sepBy pExpr sc)
    listOfParens =
      [ ('(', ')', T.SExp)
      , ('[', ']', T.List)
      , ('{', '}', T.Dict)
      ]

pExpr :: Parser T.Lisp
pExpr = choice [pInt, pFloat, pString, pSymbol, pKeyword, pLists] <?> "Expression"

pProgram :: Parser T.Lisp
pProgram = pExpr

parse :: Text -> Either Text T.Lisp
parse prog = case runParser pProgram "stdin" prog of
  (Left bundle) -> Left (toText (errorBundlePretty bundle))
  (Right v) -> Right v
