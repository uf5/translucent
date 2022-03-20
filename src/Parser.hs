module Parser (readScript) where

import Numeric (readFloat)
import Text.ParserCombinators.Parsec
import Types

allowedChar :: Parser Char
allowedChar = noneOf "\n\r \"();[\\]{}"

sep :: Parser String
sep = many $ oneOf " \n" <|> char ';' <* many (noneOf "\n")

parseSeq :: Char -> Char -> Parser [LispVal]
parseSeq open close = char open *> sep *> many (form <* sep) <* char close

stringChar :: Parser Char
stringChar = escapeChar <$> (char '\\' *> anyChar) <|> noneOf "\""

escapeChar :: Char -> Char
escapeChar 'n' = '\n'
escapeChar 'r' = '\r'
escapeChar x = x

form :: Parser LispVal
form =
  (String <$> (char '"' *> many stringChar <* char '"'))
    <|> (Keyword <$> (char ':' *> many1 allowedChar))
    <|> try
      ( Float . read <$> do
          x <- many1 digit
          char '.'
          y <- many1 digit
          return $ x ++ "." ++ y
      )
    <|> (Int . read <$> many1 digit)
    <|> try (Tuple <$> (char '(' *> sep *> parseSeq ',' ')'))
    <|> (SExp <$> parseSeq '(' ')')
    <|> (List <$> parseSeq '[' ']')
    <|> (Set <$> parseSeq '{' '}')
    <|> (prefix "quote" <$> (char '\'' *> sep *> form))
    <|> (prefix "quasiquote" <$> (char '`' *> sep *> form))
    <|> (symbol <$> many1 allowedChar)

readForm :: Parser LispVal
readForm = sep *> form

prefix :: String -> LispVal -> LispVal
prefix x y = SExp [Symbol x, y]

symbol :: String -> LispVal
symbol "True" = Bool True
symbol "False" = Bool False
symbol "None" = None
symbol x = Symbol x

readScript :: String -> [LispVal]
readScript s = case parse (many readForm) "Lisp" s of
  Left err -> (error . show) err
  Right val -> val
