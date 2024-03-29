{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Language.Translucent.LispParser (Lisp (..), Lisp' (..), parseProgram) where

import Control.Monad (void)
import Data.Char qualified as C
import Data.List (foldl1', singleton)
import Language.Translucent.Parser

type PC a = Parser Char a

data Lisp
  = None
  | Bool Bool
  | Int Integer
  | Float Float
  | String String
  | Keyword String
  | Symbol String
  | SExp [Lisp']
  | Brackets [Lisp']
  | Braces [Lisp']
  deriving (Show)

data Lisp' = Lisp' {expr :: Lisp, loc :: Location}

instance Show Lisp' where
  show (Lisp' v _) = show v

sc :: PC ()
sc =
  void $
    many $
      foldl1'
        (<|>)
        [ oneOf " \n\r\t",
          char ';' <* many (satisfy ('\n' /=))
        ]

allowedChar :: PC Char
allowedChar = noneOf " \n\r\t\"\\#,:;()[]{}"

sign :: PC (Maybe Char)
sign = optional (char '-')

number :: PC Char
number = oneOf "0123456789"

pNone :: PC Lisp
pNone = None <$ str "None"

pBool :: PC Lisp
pBool = (Bool True <$ str "True") <|> (Bool False <$ str "False")

pInt :: PC Lisp
pInt =
  Int . read <$> do
    s <- sign
    n <- some number
    pure (maybe n ((n <>) . singleton) s)

pFloat :: PC Lisp
pFloat =
  Float . read <$> do
    s <- sign
    int <- some number
    char '.'
    frac <- some number
    let woSign = int <> "." <> frac
    pure (maybe woSign ((woSign <>) . singleton) s)

pSymbol :: PC Lisp
pSymbol = Symbol <$> some allowedChar

pKeyword :: PC Lisp
pKeyword = Keyword <$> (char ':' *> some allowedChar)

pString :: PC Lisp
pString = String <$> (char '"' *> many stringChar <* char '"')
  where
    stringChar = escapeSeq <|> noneOf "\""
    escapeSeq = char '\\' *> foldl1' (<|>) [escapeChars, hex16Char, hex32Char]
    hex16Char = C.chr . hexIntList2Int <$> (char 'u' *> count 4 hexElem)
    hex32Char = C.chr . hexIntList2Int <$> (char 'U' *> count 8 hexElem)
    hexElem = foldl1' (<|>) (zipWith (\c n -> n <$ char c) "0123456789abcdef" [0 .. 15])
    hexIntList2Int :: [Int] -> Int
    hexIntList2Int (h : t) = (h * (16 ^ length t)) + hexIntList2Int t
    hexIntList2Int [] = 0
    escapeChars =
      foldl1'
        (<|>)
        ( map
            (\(c, c') -> c' <$ char c)
            [ ('\\', '\\'),
              ('\"', '\"'),
              ('a', '\a'),
              ('b', '\b'),
              ('f', '\f'),
              ('n', '\n'),
              ('r', '\r'),
              ('t', '\t'),
              ('v', '\v')
            ]
        )

pSExp :: PC Lisp
pSExp = SExp <$> (char '(' *> some pExpr <* char ')')

pBrackets :: PC Lisp
pBrackets = Brackets <$> (char '[' *> many pExpr <* char ']')

pBraces :: PC Lisp
pBraces = Braces <$> (char '{' *> many pExpr <* char '}')

pExpr :: PC Lisp'
pExpr =
  wrap $
    foldl1'
      (<|>)
      [ pFloat,
        pInt,
        pSExp,
        pBrackets,
        pBraces,
        pString,
        pNone,
        pBool,
        pKeyword,
        pSymbol
      ]
      <* sc
  where
    wrap p = do
      l <- getLocation
      v <- p
      pure (Lisp' v l)

pProgram :: PC [Lisp']
pProgram = sc *> many pExpr

parseProgram :: String -> Either (ParseError' Char) [Lisp']
parseProgram = runParser pProgram
