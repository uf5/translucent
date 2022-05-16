module Parser (readScript) where

import Text.ParserCombinators.Parsec
import Types
import Util

allowedChar :: Parser Char
allowedChar = noneOf "\n\r \"#(),;[\\]{}"

sep :: Parser String
sep = many $ oneOf " \n" <|> char ';' <* many (noneOf "\n")

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
  sep *> (String <$> (char '"' *> many stringChar <* char '"'))
    <|> (Keyword <$> (char ':' *> many1 allowedChar))
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
symbol x = Symbol x

hashed :: LispVal -> LispVal
hashed (SExp values) = Tuple values
hashed exp = error $ "unknown hashed expression: " ++ show exp

readScript :: String -> [LispVal]
readScript s = case parse (many expr) "lisp" s of
  Left err -> (error . show) err
  Right val -> val
