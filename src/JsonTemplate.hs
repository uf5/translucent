{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module JsonTemplate (jsonFields) where

import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Data (Data (toConstr), showConstr)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import PythonAst (Expression)

jsonFields :: Name -> Maybe String -> [(Name, [String])] -> Q [Dec]
jsonFields typeName tagNameOverride fields = do
  d <-
    instanceD
      (cxt [])
      (appT (conT ''ToJSON) (conT typeName))
      [funD 'toJSON (map genClause fields)]
  return [d]
  where
    genClause (n, strNames) = do
      let names = map mkNameS strNames
      return $
        Clause
          [ConP n (map VarP names)]
          ( NormalB $
              AppE
                (VarE 'object)
                ( ListE $
                    (++)
                      -- field names
                      ( zipWith
                          ( \a b ->
                              jsonEq (fs $ ls a) (VarE b)
                          )
                          strNames
                          names
                      )
                      -- tag
                      [ jsonEq
                          (fs (ls "tag"))
                          ( fs $
                              ls $
                                case tagNameOverride of
                                  (Just tag) -> tag
                                  Nothing -> nameBase n
                          )
                      ]
                )
          )
          []
    fs = AppE $ VarE 'fromString
    ls = LitE . StringL
    jsonEq a b = InfixE (Just a) (VarE '(.=)) (Just b)
