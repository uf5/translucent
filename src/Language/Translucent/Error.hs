module Language.Translucent.Error (
  Location (..),
  GeneralError (..),
  displayErrorInCode,
)
where

-- Left to right location
newtype Location = Location Int

data GeneralError = GeneralError {message :: String, location :: Location}

instance Show GeneralError where
  show (GeneralError m (Location l)) = m <> " at " <> show l

displayErrorInCode :: GeneralError -> String -> String
displayErrorInCode _ _ = undefined
