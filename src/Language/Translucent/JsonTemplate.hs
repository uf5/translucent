{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Translucent.JsonTemplate (jsonFields, jsonDataTag) where

import Data.Aeson
import Data.Aeson.Key (fromString)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

jsonFields :: Name -> Maybe String -> [(Name, [String])] -> Q [Dec]
jsonFields typeName tagNameOverride fields = do
  d <-
    instanceD
      (cxt [])
      (appT (conT ''ToJSON) (conT typeName))
      [funD 'toJSON (map genClause fields)]
  return [d]
  where
    genClause (n, strNames) =
      let names = map mkNameS strNames
       in return $
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
                              (fs $ ls "tag")
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

jsonDataTag :: Name -> Q [Dec]
jsonDataTag typeName = do
  TyConI (DataD _ _ _ _ constrs _) <- reify typeName
  d <-
    instanceD
      (cxt [])
      (appT (conT ''ToJSON) (conT typeName))
      [funD 'toJSON (map genClause constrs)]
  return [d]
  where
    genClause (NormalC c []) =
      return $
        Clause
          [ConP c []]
          ( NormalB $
              AppE
                (VarE 'object)
                ( ListE
                    [ InfixE
                        (Just $ fs $ ls "tag")
                        (VarE '(.=))
                        (Just $ fs $ ls $ nameBase c)
                    ]
                )
          )
          []
    genClause _ = error "not valid"
    fs = AppE $ VarE 'fromString
    ls = LitE . StringL
