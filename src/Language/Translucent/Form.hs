{-# LANGUAGE LambdaCase #-}

module Language.Translucent.Form (
  Form (..),
  module A,
  satisfy,
  applyArgs,
  FormApplicationError (..),
)
where

import Control.Applicative (Alternative (..))
import Control.Applicative as A (Alternative (..), many, optional, some)

data FormApplicationError
  = NotEnoughArguments
  | TooManyArguments
  | WrongType
  | EmptyParam

instance Show FormApplicationError where
  show NotEnoughArguments = "Not enough arguments"
  show TooManyArguments = "Too many arguments"
  show WrongType = "Wrong param type in param"
  show EmptyParam = "Got an empty param"

newtype Form i a = Form
  { applyArgs' :: [i] -> Either FormApplicationError (a, [i])
  }

instance Functor (Form i) where
  fmap f (Form x) = Form $ \s -> do
    (x', s') <- x s
    return (f x', s')

instance Applicative (Form i) where
  pure x = Form $ \s -> pure (x, s)
  (Form f) <*> (Form x) = Form $ \s -> do
    (f', s1) <- f s
    (x', s2) <- x s1
    return (f' x', s2)

instance Monad (Form i) where
  (Form x) >>= f = Form $ \s -> do
    (x', s') <- x s
    applyArgs' (f x') s'

instance Alternative (Form i) where
  empty = Form (const (Left EmptyParam))
  (Form a) <|> (Form b) = Form $ \s -> a s <> b s

satisfy :: (i -> Maybe a) -> Form i a
satisfy predicate = Form $ \case
  [] -> Left NotEnoughArguments
  hd : rest -> case predicate hd of
    Just x -> pure (x, rest)
    Nothing -> Left WrongType

applyArgs :: Form i a -> [i] -> Either FormApplicationError a
applyArgs f s = case applyArgs' f s of
  Left e -> Left e
  Right (a, []) -> pure a
  Right (_, _) -> Left TooManyArguments
