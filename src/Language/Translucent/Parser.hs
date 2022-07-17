module Language.Translucent.Parser (readProgram) where

import Data.Text (Text, pack)
import Data.Void (Void)
import Language.Translucent.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

allowedChar :: Parser Char
allowedChar = noneOf "\n\r\t \"#(),;[\\]{}"

stringChar :: Parser Char
stringChar = escapeChar <|> anySingleBut '"'

escapeChar :: Parser Char
escapeChar =
  char '\\'
    *> choice
      [ esc '\n' 'n',
        esc '\r' 'r',
        same '\\',
        same '"'
      ]
  where
    esc :: Char -> Char -> Parser Char
    esc x y = x <$ char y
    same x = esc x x

hashed :: Lisp -> Lisp
hashed (SExp values) = Tuple values
hashed (Set values) = mkDict values
  where
    mkDict :: [Lisp] -> Lisp
    mkDict x
      | even (length x) = let (values, keys) = evensAndOdds x in Dict keys values
      | otherwise = undefined
      where
        evensAndOdds :: [a] -> ([a], [a])
        evensAndOdds = foldr f ([], [])
          where
            f a (ls, rs) = (rs, a : ls)
hashed _ = undefined

pExpr :: Parser Lisp
pExpr =
  choice
    [ String . pack <$> (char '"' *> many stringChar <* char '"') <?> "String",
      Float <$> try (L.signed (return ()) L.float) <?> "Float",
      Int <$> try (L.signed (return ()) L.decimal) <?> "Int",
      SExp <$> parseSeq "(" ")" <?> "S-Expression",
      List <$> parseSeq "[" "]" <?> "List",
      Set <$> parseSeq "{" "}" <?> "Set",
      hashed <$> (char '#' *> pExpr) <?> "Hashed expression",
      prefix "quote" <$> (char '\'' *> pExpr) <?> "Quoted expression",
      symbol <$> some allowedChar <?> "Symbol"
    ]
    <?> "Expression"
  where
    parseSeq :: String -> String -> Parser [Lisp]
    parseSeq open close = string open *> sc *> many (pExpr <* sc) <* string close
    prefix x y = SExp [Symbol (pack x), y]

symbol :: String -> Lisp
symbol "None" = None
symbol "True" = Bool True
symbol "False" = Bool False
symbol x = Symbol (pack x)

pProgram :: Parser [Lisp]
pProgram = sc *> many (pExpr <* sc) <* eof

readProgram = parse pProgram
