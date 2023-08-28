module Transpiler.Mangler (mangle) where

import Data.Char (GeneralCategory (..), generalCategory)
import Data.Map
import Optics
import Text.Megaparsec qualified as M
import Transpiler.Monad

mangle :: Text -> Transpiler Text
mangle x = do
  nms <- use (manglerState % definedNames)
  maybe (mangleAndInsert x nms) pure (lookup x nms)
  where
    mangleAndInsert originalName nms = do
      mangledName <- makeValidPythonId originalName
      assign (manglerState % definedNames) (insert originalName mangledName nms)
      pure mangledName

makeValidPythonId :: Text -> Transpiler Text
makeValidPythonId originalName
  | isValidPythonId originalName = pure originalName
  | otherwise = do
      prefix <- use (config % manglerPrefix)
      c <- length <$> use (manglerState % definedNames)
      pure (prefix <> show c)

isValidPythonId :: Text -> Bool
isValidPythonId x = isRight (M.runParser (pyIdentParser <* M.eof) "" x)
  where
    pyIdentParser :: M.Parsec Void Text ()
    pyIdentParser = do
      _ <- charCategories charStart
      _ <- M.many (charCategories charContinue)
      pure ()
    charCategories categories = M.satisfy ((`elem` categories) . generalCategory)
    charStart =
      [ UppercaseLetter
      , LowercaseLetter
      , TitlecaseLetter
      , ModifierLetter
      , OtherLetter
      , LetterNumber
      ]
    charContinue =
      charStart
        ++ [ NonSpacingMark
           , SpacingCombiningMark
           , DecimalNumber
           , ConnectorPunctuation
           ]
