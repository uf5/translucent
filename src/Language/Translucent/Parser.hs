module Language.Translucent.Parser (readProgram) where

import Data.Text (Text, pack)
import Language.Translucent.Lisp
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Language.Translucent.ParserTypes

type LParser a = Parser (Location -> a)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

withLocation :: Parser (Location -> a) -> Parser a
withLocation parser = do
  o <- getOffset
  p <- parser
  return $ p (Location o)

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

parseSeq open close = char open *> sc *> many (pExpr <* sc) <* char close

prefix x expr loc = SExp loc [Symbol Generated (pack x), expr]

strTypeHelper x str loc = x loc (pack str)

pSExp :: LParser Lisp
pSExp = flip SExp <$> parseSeq '(' ')' <?> "SExp"

pList :: LParser Lisp
pList = flip List <$> parseSeq '[' ']' <?> "List"

pDict :: LParser Lisp
pDict = (\elts loc -> uncurry (Dict loc) (evensAndOdds elts)) <$> parseSeq '{' '}'
  where
    evensAndOdds :: [a] -> ([a], [a])
    evensAndOdds = foldr f ([], [])
      where
        f a (ls, rs) = (a : rs, ls)

pInt :: LParser Lisp
pInt = flip Int <$> try (L.signed (return ()) L.decimal) <?> "Float"

pFloat :: LParser Lisp
pFloat = flip Float <$> try (L.signed (return ()) L.float) <?> "Float"

pSymbol :: LParser Lisp
pSymbol = specialSymbols <$> some allowedChar <?> "Symbol"
  where
    specialSymbols "True" loc = Bool loc True
    specialSymbols "False" loc = Bool loc False
    specialSymbols "None" loc = None loc
    specialSymbols str loc = Symbol loc (pack str)

pString :: LParser Lisp
pString = strTypeHelper String <$> (char '"' *> many stringChar <* char '"') <?> "String"

pKeyword :: LParser Lisp
pKeyword = strTypeHelper Keyword <$> (char ':' *> some allowedChar) <?> "Keyword"

pQuote :: LParser Lisp
pQuote = prefix "quote!" <$> (char '\'' *> pExpr) <?> "Quoted expression"

pExpr :: Parser Lisp
pExpr =
  withLocation
    ( choice
        [ pString,
          pFloat,
          pInt,
          pSExp,
          pList,
          pDict,
          pQuote,
          pKeyword,
          pSymbol
        ]
        <?> "Expression"
    )

pProgram :: Parser [Lisp]
pProgram = sc *> many (pExpr <* sc) <* eof

readProgram = parse pProgram
