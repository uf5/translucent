module Language.Translucent.Types where

import Data.Text (Text, unpack)
import Text.Megaparsec (SourcePos)

data Location
  = Location {start :: SourcePos, end :: SourcePos}
  | Generated

data Lisp
  = None Location
  | Bool Location Bool
  | Int Location Integer
  | Float Location Float
  | String Location Text
  | Symbol Location Text
  | Keyword Location Text
  | SExp Location [Lisp]
  | List Location [Lisp]
  | Tuple Location [Lisp]
  | Set Location [Lisp]
  | Dict Location [Lisp] [Lisp]

instance Show Lisp where
  show (None _) = "None"
  show (Bool _ x) = show x
  show (Int _ x) = show x
  show (Float _ x) = show x
  show (String _ x) = show x
  show (Symbol _ x) = unpack x
  show (Keyword _ x) = ":" ++ unpack x
  show (SExp _ x) = "(" ++ unwords (map show x) ++ ")"
  show (List _ x) = "[" ++ unwords (map show x) ++ "]"
  show (Tuple _ x) = "#(" ++ unwords (map show x) ++ ")"
  show (Set _ x) = "{" ++ unwords (map show x) ++ "}"
  show (Dict _ keys values) = "#{" ++ unwords (zipWith (\k v -> show k ++ " " ++ show v) keys values) ++ "}"
