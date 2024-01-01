module Language.Translucent.Error (
  GeneralError (..),
  geIncrementErrorLocation,
  displayErrorInCode,
  pe2ge,
  te2ge,
)
where

import Language.Translucent.Parser (Location (..))
import Language.Translucent.Parser qualified as P (ParseError' (..))
import Language.Translucent.TransM qualified as T (TranspilerError' (..))

data GeneralError = GeneralError {message :: String, location :: Location}

instance Show GeneralError where
  show (GeneralError m (Location l)) = m <> " at " <> show l

pe2ge :: (Show i) => P.ParseError' i -> GeneralError
pe2ge
  ( P.ParseError'
      { P.err = e,
        P.loc = l
      }
    ) = GeneralError (show e) l

te2ge :: T.TranspilerError' -> GeneralError
te2ge
  ( T.TranspilerError'
      { T.err = e,
        T.loc = l
      }
    ) = GeneralError (show e) l

geIncrementErrorLocation :: GeneralError -> GeneralError
geIncrementErrorLocation g@GeneralError {location = (Location l)} =
  g {location = Location (l + 1)}

displayErrorInCode :: String -> GeneralError -> String
displayErrorInCode
  prog
  GeneralError
    { message = m,
      location = (Location l)
    } = lineNumberStr <> line <> "\n" <> replicate (length lineNumberStr + lineChar) ' ' <> "^\n" <> m
    where
      lineNumberStr = show (lineIdx + 1) <> " | "
      line = lines prog !! lineIdx
      lineIdx = fst lineData
      lineChar = snd lineData
      lineData = findLocationLine (map length (lines prog))
      findLocationLine = findLocationLine' 0 0
      findLocationLine' idx sm (h : t)
        | (sm + h) >= l = (idx, l - sm - idx)
        | otherwise = findLocationLine' (idx + 1) (sm + h) t
      findLocationLine' _ _ [] = undefined
