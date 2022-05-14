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
    genClause (n, strNames) = do
      let names = map mkNameS strNames
      return $
        Clause
          [ConP n (map VarP names)]
          ( NormalB $
              AppE
                (VarE 'object)
                ( ListE $
                    zipWith
                      ( \a b ->
                          jsonEq (AppE fs (ls a)) (VarE b)
                      )
                      strNames
                      names
                      ++ [ jsonEq
                             (AppE fs (ls "tag"))
                             (AppE fs (ls $ nameBase n))
                         ]
                )
          )
          []
    fs = VarE 'fromString
    ls = LitE . StringL
    jsonEq a b = InfixE (Just a) (VarE '(.=)) (Just b)
