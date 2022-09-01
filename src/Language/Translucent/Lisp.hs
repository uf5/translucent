module Language.Translucent.Lisp
  ( Location (..),
    Lisp (..),
    getLoc,
  )
where

import Data.Text (Text, unpack)
import Data.Void (Void)
import Text.Megaparsec (PosState)
import GHC.Exception (underflowException)

data Location
  = Location {offset :: Int}
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
  show (Set _ x) = "{" ++ unwords (map show x) ++ "}"
  show (Dict _ keys values) = "#{" ++ unwords (zipWith (\k v -> show k ++ " " ++ show v) keys values) ++ "}"

getLoc :: Lisp -> Location
getLoc (None loc) = loc
getLoc (Bool loc _) = loc
getLoc (Int loc _) = loc
getLoc (Float loc _) = loc
getLoc (String loc _) = loc
getLoc (Symbol loc _) = loc
getLoc (Keyword loc _) = loc
getLoc (SExp loc _) = loc
getLoc (List loc _) = loc
getLoc (Set loc _) = loc
getLoc (Dict loc _ _) = loc
