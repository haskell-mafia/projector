{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Pretty (
    ppType
  , ppExpr
  ) where


import qualified Data.Text as T

import           P

import           Projector.Core.Syntax (Expr (..), Name (..), subst)
import           Projector.Core.Type (Type (..), Ground (..))


ppType :: Ground l => Type l -> Text
ppType t =
  case t of
    TLit g ->
      ppGroundType g

    TArrow a b ->
      "(" <> ppType a <> " -> " <> ppType b <> ")"

ppExpr :: Ground l => Expr l -> Text
ppExpr e =
  case e of
    EVar (Name n) ->
      n

    ELit b ->
      ppGroundValue b

    EApp f g ->
      let ff = ppExpr f
          gg = ppExpr g
      in parenMay ff <> " " <> parenMay gg

    ELam ss (Name n) t f ->
      "\\" <> n <> " : " <> ppType t <> ". " <> ppExpr (subst ss f)

hasSpace :: Text -> Bool
hasSpace =
  isJust . T.find (== ' ')

parenMay :: Text -> Text
parenMay t =
  if hasSpace t then "(" <> t <> ")" else t
