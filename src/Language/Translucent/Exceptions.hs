{-# LANGUAGE FlexibleContexts #-}

module Language.Translucent.Exceptions where

import Control.Monad.Except
import Data.Ord
import Language.Translucent.Types
import Text.Megaparsec.Pos

type TransExceptT m = ExceptT TransError m

newtype TransError = TransError
  { message :: String
  }

getLocation :: Lisp -> Location
getLocation (None loc) = loc
getLocation (Bool loc _) = loc
getLocation (Int loc _) = loc
getLocation (Float loc _) = loc
getLocation (String loc _) = loc
getLocation (Symbol loc _) = loc
getLocation (Keyword loc _) = loc
getLocation (SExp loc _) = loc
getLocation (List loc _) = loc
getLocation (Tuple loc _) = loc
getLocation (Set loc _) = loc
getLocation (Dict loc _ _) = loc

throwTransError expr message =
  throwError
    ( TransError
        ( message <> "\n" <> case getLocation expr of
            Location {start = s, end = e} ->
              sourceName s
                <> ": ("
                <> showLine s
                <> ","
                <> showCol s
                <> ")"
                <> "-("
                <> showLine e
                <> ","
                <> showCol e
                <> ")"
            Generated -> "In generated expression: " <> show expr
        )
    )
  where
    showPos = show . unPos
    showLine = showPos . sourceLine
    showCol = showPos . sourceColumn
