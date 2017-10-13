{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Core.Machinator (
    machinatorDecls
  , fromMachinator
  ) where


import           Machinator.Core (Definition (..))
import qualified Machinator.Core.Data.Definition as MC

import           P

import           Projector.Core
import qualified Projector.Html.Core.Prim as Prim
import           Projector.Html.Data.Prim


-- | Convert a set of Machinator definitions to a Projector
-- declaration set.
machinatorDecls :: Foldable f => f (Definition a) -> HtmlDecls a
machinatorDecls =
  foldl' (\decls def -> uncurry declareType (fromMachinator def) decls) mempty
{-# SPECIALIZE machinatorDecls :: [Definition a] -> HtmlDecls a #-}

-- | Convert a Machinator definition to a Projector definition.
fromMachinator :: Definition a -> (TypeName, HtmlDecl a)
fromMachinator (Definition (MC.Name n) dt _a) =
  (TypeName n, fromMachinatorDT dt)

fromMachinatorDT :: MC.DataType a -> HtmlDecl a
fromMachinatorDT dt =
  case dt of
    MC.Variant nts a ->
      DVariant
        (toList . with nts $ \(MC.Name n, ts) -> (Constructor n, fmap fromMachinatorT ts))
        a
    MC.Record fts a ->
      DRecord
        (with fts $ \(MC.Name n, t) -> (FieldName n, fromMachinatorT t))
        a

fromMachinatorT :: MC.Type -> HtmlType
fromMachinatorT mt =
  case mt of
    MC.Variable (MC.Name n) ->
      TVar (TypeName n)
    MC.GroundT g ->
      fromMachinatorGT g
    MC.ListT t ->
      TList (fromMachinatorT t)

fromMachinatorGT :: MC.Ground -> HtmlType
fromMachinatorGT g =
  case g of
    MC.StringT ->
      TLit TString
    MC.BoolT ->
      TVar Prim.nBool
