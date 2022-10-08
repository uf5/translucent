{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Language.Translucent.Params
  ( ParamError (..),
    Param (..),
    Alternative (..),
  )
where

import Control.Applicative (Alternative (..))
import Debug.Trace (trace, traceStack)
import Language.Translucent.Lisp

data ParamError
  = NotEnoughArguments
  | TooManyArguments
  | WrongType Location
  | EmptyParam

instance Show ParamError where
  show NotEnoughArguments = "Not enough arguments"
  show TooManyArguments = "Too many arguments"
  show (WrongType loc) = "Wrong param type"
  show EmptyParam = "Got an empty param"

newtype Param a = Param {applyArgs :: [Lisp] -> Either ParamError (a, [Lisp])}

instance Functor Param where
  fmap f (Param x) = Param $ \s -> do
    (x', s') <- x s
    return (f x', s')

instance Applicative Param where
  pure x = Param $ \s -> Right (x, s)
  (Param f) <*> (Param x) = Param $ \s -> do
    (f', s1) <- f s
    (x', s2) <- x s1
    return (f' x', s2)

instance Monad Param where
  (Param x) >>= f = Param $ \s -> do
    (x', s') <- x s
    applyArgs (f x') s'

instance Alternative Param where
  empty = Param $ const $ Left EmptyParam
  (Param a) <|> (Param b) = Param $ \s ->
    case a s of
      x@(Right _) -> x
      (Left _) -> b s
  some v = some_v
    where
      many_v = some_v <|> pure []
      some_v = (:) <$> v <*> many_v
  many v = many_v
    where
      many_v = some_v <|> pure []
      some_v = (:) <$> v <*> many_v
