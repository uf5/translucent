{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module JsonTemplate (jsonFields) where

import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Data (Data (toConstr), showConstr)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import PythonAst (Expression)

jsonFields :: Name -> [(Name, [String])] -> Q [Dec]
jsonFields typeName fields = do
  d <-
    instanceD
      (cxt [])
      (appT (conT ''ToJSON) (conT typeName))
      [funD 'toJSON (map genClause fields)]
  return [d]
  where
    genClause (n, fieldsStr) = do
      let names = map mkNameS fieldsStr
      return $
        Clause
          [ConP n (map VarP names)]
          ( NormalB $
              AppE
                (VarE 'object)
                ( ListE
                    ( zipWith
                        ( \a b ->
                            InfixE
                              (Just $ AppE (VarE 'fromString) (LitE $ StringL a))
                              (VarE '(.=))
                              (Just $ VarE b)
                        )
                        fieldsStr
                        names
                        ++ [ InfixE
                               (Just $ AppE (VarE 'fromString) (LitE $ StringL "tag"))
                               (VarE '(.=))
                               (Just $ AppE (VarE 'fromString) (LitE $ StringL $ showConstr $ toConstr n))
                           ]
                    )
                )
          )
          []
