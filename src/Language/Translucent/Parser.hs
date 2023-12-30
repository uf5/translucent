{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Language.Translucent.Parser (
  module A,
  RLocation (..),
  Parser (..),
  ParseError (..),
  ParseError' (..),
  pe2ge,
  getRLocation,
  satisfy,
  char,
  str,
  oneOf,
  noneOf,
  choice,
  count,
  eof,
  runParser,
)
where

import Control.Applicative
import Control.Applicative as A (Alternative (..), many, optional, some)
import Control.Monad (replicateM, when)
import Language.Translucent.Error (GeneralError (..), Location (..))

-- Right to left location
newtype RLocation = RLocation Int

data ParseError' i = ParseError'
  { err :: ParseError i
  , loc :: RLocation
  }

pe2ge :: (Show i) => Int -> ParseError' i -> GeneralError
pe2ge
  sourceLength
  ( ParseError'
      { err = e
      , loc = (RLocation rl)
      }
    ) = GeneralError (show e) (Location (sourceLength - rl))

instance (Show i) => Show (ParseError' i) where
  show ParseError' {err = e} = show e

data ParseError i
  = Unexpected i
  | ExpectedEOF
  | EOF
  | Empty
  deriving (Show)

newtype Parser i a = Parser {runParser' :: [i] -> Either (ParseError' i) (a, [i])}

instance Functor (Parser i) where
  fmap f (Parser p) = Parser $ \inp -> do
    (output, rest) <- p inp
    pure (f output, rest)

instance Applicative (Parser i) where
  pure x = Parser $ \inp -> pure (x, inp)
  Parser f <*> Parser p = Parser $ \inp -> do
    (f', rest) <- f inp
    (output, rest') <- p rest
    pure (f' output, rest')

instance Monad (Parser i) where
  return = pure
  Parser p >>= f = Parser $ \inp -> do
    (out, rest) <- p inp
    runParser' (f out) rest

instance Alternative (Parser i) where
  empty = do
    l <- getRLocation
    Parser (const (Left (ParseError' Empty l)))
  Parser a <|> Parser b = Parser $ \inp ->
    case (a inp, b inp) of
      (r@(Right _), _) -> r
      (Left _, r@(Right _)) -> r
      (l@(Left _), Left _) -> l

-- Get the current position within the source list. Keep in mind that this position is equal to the index counted from the end of the list.
getRLocation :: Parser i RLocation
getRLocation = Parser $ \inp -> pure (RLocation (length inp), inp)

satisfy :: (i -> Bool) -> Parser i i
satisfy predicate = do
  l <- getRLocation
  Parser $ \inp -> case inp of
    [] -> Left (ParseError' EOF l)
    hd : rest
      | predicate hd -> Right (hd, rest)
      | otherwise -> Left (ParseError' (Unexpected hd) l)

char :: Eq i => i -> Parser i i
char c = satisfy (c ==)

str :: (Traversable t, Eq i) => t i -> Parser i (t i)
str = mapM char

oneOf :: (Foldable f, Eq i) => f i -> Parser i i
oneOf elts = satisfy (`elem` elts)

noneOf :: (Foldable f, Eq i) => f i -> Parser i i
noneOf elts = satisfy (not . (`elem` elts))

choice :: Foldable f => f (Parser i a) -> Parser i a
choice = foldl1 (<|>)

count :: Int -> Parser i a -> Parser i [a]
count = replicateM

eof :: Parser i ()
eof = do
  l@(RLocation n) <- getRLocation
  when (n /= 0) (Parser (const (Left (ParseError' ExpectedEOF l))))

runParser :: Parser i a -> [i] -> Either (ParseError' i) a
runParser p source = fst <$> runParser' (p <* eof) source
