{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Language.Translucent.LispParser (Lisp (..), Lisp' (..), parseProgram) where

import Control.Monad (void)
import Data.List (singleton)
import Language.Translucent.Parser

type PC a = Parser Char a

data Lisp
  = Int Integer
  | Float Float
  | Symbol String
  | Keyword String
  | SExp [Lisp']
  | List [Lisp']
  deriving (Show)

newtype Lisp' = Lisp' (Lisp, Location)

instance Show Lisp' where
  show (Lisp' (v, _)) = show v

sc :: PC ()
sc =
  void $
    many $
      choice
        [ oneOf " \n\r\t",
          char ';' <* many (satisfy ('\n' /=))
        ]

allowedChar :: PC Char
allowedChar = noneOf " \n\r\t\"\\#,;()[]{}"

sign :: PC (Maybe Char)
sign = optional (char '-')

number :: PC Char
number = oneOf "0123456789"

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

pExpr :: PC Lisp'
pExpr = wrap $ choice [pFloat, pInt, pSymbol] <* sc
  where
    wrap p = do
      l <- getLocation
      v <- p
      pure (Lisp' (v, l))

pProgram :: PC [Lisp']
pProgram = many pExpr

parseProgram :: String -> Either (ParseError' Char) [Lisp']
parseProgram = runParser pProgram
