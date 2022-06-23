{-# LANGUAGE OverloadedStrings #-}

module Language.Translucent.Parser (readProgram) where

import Data.Text (Text, pack)
import Language.Translucent.Types
import Text.ParserCombinators.Parsec

allowedChar :: Parser Char
allowedChar = noneOf "\n\r\t \"#(),;[\\]{}"

sep :: Parser String
sep = many $ oneOf " \n\t" <|> char ';' <* many (noneOf "\n")

parseSeq :: Char -> Char -> Parser [LispVal]
parseSeq open close = char open *> sep *> many (expr <* sep) <* char close

stringChar :: Parser Char
stringChar = escapeChar <$> (char '\\' *> anyChar) <|> noneOf "\""

escapeChar :: Char -> Char
escapeChar 'n' = '\n'
escapeChar 'r' = '\r'
escapeChar x = error ("Unknown escape character: " ++ show x)

expr :: Parser LispVal
expr =
  (String . pack <$> (char '"' *> many stringChar <* char '"'))
    <|> (Keyword . pack <$> (char ':' *> many1 allowedChar))
    <|> try
      ( Float . read <$> do
          x <- many1 digit
          char '.'
          y <- many1 digit
          return $ x ++ "." ++ y
      )
    <|> (Int . read <$> many1 digit)
    <|> (SExp <$> parseSeq '(' ')')
    <|> (List <$> parseSeq '[' ']')
    <|> (Set <$> parseSeq '{' '}')
    <|> (hashed <$> (char '#' *> expr))
    <|> (prefix "quote" <$> (char '\'' *> expr))
    <|> (prefix "quasiquote" <$> (char '`' *> expr))
    <|> (symbol <$> many1 allowedChar)

symbol :: String -> LispVal
symbol "True" = Bool True
symbol "False" = Bool False
symbol "None" = None
symbol x = Symbol $ pack x

hashed :: LispVal -> LispVal
hashed (SExp values) = Tuple values
hashed x = error $ "unknown hashed expression: " ++ show x

program :: Parser [LispVal]
program = sep *> many (expr <* sep) <* eof

readProgram = parse program

prefix :: Text -> LispVal -> LispVal
prefix x y = SExp [Symbol x, y]
