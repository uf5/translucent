{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Language.Translucent.Parser (
  module A,
  Location (..),
  ParserState (..),
  Parser (..),
  ParseError (..),
  ParseError' (..),
  getLocation,
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

newtype Location = Location Int

data ParseError' i = ParseError'
  { err :: ParseError i,
    loc :: Location
  }

instance (Show i) => Show (ParseError' i) where
  show ParseError' {err = e} = show e

data ParseError i
  = Unexpected i
  | ExpectedEOF
  | EOF
  | Empty
  deriving (Show)

data ParserState i = ParserState
  { source :: [i],
    initialSourceLength :: Int
  }

newtype Parser i a = Parser
  { runParser' :: ParserState i -> Either (ParseError' i) (a, ParserState i)
  }

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
    l <- getLocation
    Parser (const (Left (ParseError' Empty l)))
  Parser a <|> Parser b = Parser $ \inp ->
    case (a inp, b inp) of
      (r@(Right _), _) -> r
      (Left _, r@(Right _)) -> r
      (l@(Left _), Left _) -> l

gets :: (ParserState i -> b) -> Parser i b
gets fn = Parser $ \s -> pure (fn s, s)

getLocation :: Parser i Location
getLocation = do
  src <- gets source
  isl <- gets initialSourceLength
  pure (Location (isl - length src))

satisfy :: (i -> Bool) -> Parser i i
satisfy predicate = do
  l <- getLocation
  Parser $ \s@ParserState {source = inp} -> case inp of
    [] -> Left (ParseError' EOF l)
    hd : rest
      | predicate hd -> Right (hd, s {source = rest})
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
  isl <- gets initialSourceLength
  l@(Location l') <- getLocation
  when (l' /= isl) (Parser (const (Left (ParseError' ExpectedEOF l))))

runParser :: Parser i a -> [i] -> Either (ParseError' i) a
runParser p input = fst <$> runParser' (p <* eof) ParserState {source = input, initialSourceLength = length input}
