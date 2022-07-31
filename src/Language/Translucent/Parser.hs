module Language.Translucent.Parser (readProgram) where

import Data.Char (GeneralCategory (..), generalCategory)
import qualified Data.Set as S
import Data.Text (Text, pack)
import Data.Void (Void)
import Language.Translucent.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type LParser a = Parsec Void String (Location -> a)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

withLocation :: Parser (Location -> a) -> Parser a
withLocation parser = do
  start <- getSourcePos
  p <- parser
  end <- getSourcePos
  return $ p (Location start end)

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

hashOpen = char '#'

pSExp :: LParser Lisp
pSExp = flip SExp <$> parseSeq '(' ')' <?> "SExp"

pList :: LParser Lisp
pList = flip SExp <$> parseSeq '[' ']' <?> "List"

pSet :: LParser Lisp
pSet = flip SExp <$> parseSeq '{' '}' <?> "Set"

pDict :: LParser Lisp
pDict = (\elts loc -> uncurry (Dict loc) (evensAndOdds elts)) <$> (hashOpen >> parseSeq '{' '}')
  where
    evensAndOdds :: [a] -> ([a], [a])
    evensAndOdds = foldr f ([], [])
      where
        f a (ls, rs) = (a : rs, ls)

pTuple :: LParser Lisp
pTuple = flip Tuple <$> (hashOpen >> parseSeq '(' ')') <?> "Tuple"

pInt :: LParser Lisp
pInt = flip Int <$> try (L.signed (return ()) L.decimal) <?> "Float"

pFloat :: LParser Lisp
pFloat = flip Float <$> try (L.signed (return ()) L.float) <?> "Float"

pSymbol :: LParser Lisp
pSymbol = strTypeHelper Symbol <$> some allowedChar <?> "Symbol"

pString :: LParser Lisp
pString = strTypeHelper String <$> (char '"' *> many stringChar <* char '"') <?> "String"

pKeyword :: LParser Lisp
pKeyword = strTypeHelper Keyword <$> (char ':' *> some allowedChar) <?> "Keyword"

pQuote :: LParser Lisp
pQuote = prefix "quote" <$> (char '\'' *> pExpr) <?> "Quoted expression"

pExpr :: Parser Lisp
pExpr =
  withLocation $
    choice
      [ pString,
        pFloat,
        pInt,
        pSExp,
        pList,
        pSet,
        pTuple,
        pDict,
        pQuote,
        pKeyword,
        pSymbol
      ]

pProgram :: Parser [Lisp]
pProgram = sc *> many (pExpr <* sc) <* eof

readProgram = parse pProgram
