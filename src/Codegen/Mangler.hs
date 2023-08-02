module Codegen.Mangler (pickName) where

import Codegen.Codegen
import Data.Char (GeneralCategory (..), generalCategory)
import Data.Map
import Optics
import Text.Megaparsec qualified as M

pickName :: Text -> Codegen Text
pickName x = get >>= (dispatchName . view names)
  where
    dispatchName :: Map Text Text -> Codegen Text
    dispatchName nms = case lookup x nms of
      (Just n) -> return n
      Nothing -> do
        let mangledName =
              if isValidPythonIdent x
                then x
                else mangle x (length nms)
        let newMap = insert x mangledName nms
        assign names newMap
        return mangledName

-- TODO: generate names that resemble their original names
-- example: this-is-a-test -> this_is_a_test
mangle :: Text -> Int -> Text
mangle _ c = "__tr_gen_n_" <> show c

isValidPythonIdent :: Text -> Bool
isValidPythonIdent x = isRight (M.runParser (pyIdentParser <* M.eof) "" x)
  where
    pyIdentParser :: M.Parsec Void Text ()
    pyIdentParser = do
      _ <- charCategories charStart
      _ <- M.many (charCategories charContinue)
      pure ()
    charCategories categories = M.satisfy ((`elem` categories) . generalCategory)
    charStart = [UppercaseLetter, LowercaseLetter, TitlecaseLetter, ModifierLetter, OtherLetter, LetterNumber]
    charContinue = charStart ++ [NonSpacingMark, SpacingCombiningMark, DecimalNumber, ConnectorPunctuation]
