module Language.Translucent.Parser (readScript) where

import Language.Translucent.Types
import Text.ParserCombinators.Parsec

allowedChar :: Parser Char
allowedChar = noneOf "\t\n\r \"#(),;[\\]{}"

sep :: Parser String
sep = many $ space <|> char ';' <* many (noneOf "\n")

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
  many sep
    *> ( (String <$> (char '"' *> many stringChar <* char '"'))
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
       )
    <* many sep

symbol :: String -> LispVal
symbol "True" = Bool True
symbol "False" = Bool False
symbol "None" = None
symbol x = Symbol x

hashed :: LispVal -> LispVal
hashed (SExp values) = Tuple values
hashed x = error $ "unknown hashed expression: " ++ show x

program :: Parser [LispVal]
program = many expr <* eof

readScript :: String -> [LispVal]
readScript s = case parse program "lisp" s of
  Left err -> error $ show err
  Right val -> val

prefix :: String -> LispVal -> LispVal
prefix x y = SExp [Symbol x, y]
