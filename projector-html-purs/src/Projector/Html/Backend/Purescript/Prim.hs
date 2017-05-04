{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Projector.Html.Backend.Purescript.Prim (
    PurescriptPrimT (..)
  , Value (..)
  , toPurescriptExpr
  , toPurescriptModule
  , toPurescriptTypeDecls
  , PurescriptType
  , PurescriptExpr
  , PurescriptModule
  , PurescriptDecl
  , PurescriptDecls
  ) where


import qualified Data.Text as T

import           P

import           Projector.Core
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim


type PurescriptType   = Type PurescriptPrimT
type PurescriptExpr   = Expr PurescriptPrimT
type PurescriptDecl   = Decl PurescriptPrimT
type PurescriptDecls  = TypeDecls PurescriptPrimT
type PurescriptModule = Module PurescriptType PurescriptPrimT

data PurescriptPrimT
  = PTextT
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance Ground PurescriptPrimT where
  data Value PurescriptPrimT
    = PTextV Text
    deriving (Eq, Ord, Read, Show)

  typeOf v = case v of
    PTextV _ -> PTextT

  ppGroundType t = case t of
    PTextT -> "String"

  ppGroundValue v = case v of
    PTextV s ->
      T.pack (show s)

toPurescriptExpr :: Expr PrimT a -> Expr PurescriptPrimT a
toPurescriptExpr =
  mapGround tmap vmap

toPurescriptModule :: Module HtmlType PrimT a -> Module PurescriptType PurescriptPrimT a
toPurescriptModule (Module typs imps exps) =
  Module
    (toPurescriptTypeDecls typs)
    imps
    (with exps (\(ModuleExpr t e) -> ModuleExpr (toPurescriptType t) (toPurescriptExpr e)))

toPurescriptType :: HtmlType -> PurescriptType
toPurescriptType =
  swapLibTypes . mapGroundType tmap

toPurescriptTypeDecls :: HtmlDecls -> PurescriptDecls
toPurescriptTypeDecls (TypeDecls decls) =
  TypeDecls . with decls $ \decl ->
    case decl of
      DVariant cts ->
        DVariant (with cts (fmap (fmap toPurescriptType)))
      DRecord fts ->
        DRecord (with fts (fmap toPurescriptType))

-- if we encounter library types we also have to tweak them
swapLibTypes :: PurescriptType -> PurescriptType
swapLibTypes ty =
  case ty of
    TList t2 ->
      TList (swapLibTypes t2)
    TArrow t2 t3 ->
      TArrow (swapLibTypes t2) (swapLibTypes t3)
    TVar (TypeName "Html") ->
      TVar (TypeName "(Array (Pux.Html ev))")
    TVar (TypeName "Attribute") ->
      TVar (TypeName "(Pux.Attribute ev)")
    TVar (TypeName "AttributeKey") ->
      TVar (TypeName "String")
    TVar (TypeName "AttributeValue") ->
      TVar (TypeName "String")
    TVar (TypeName "Tag") ->
      TVar (TypeName "String")
    TVar (TypeName "Bool") ->
      TVar (TypeName "Boolean")
    _ ->
      ty

tmap :: PrimT -> PurescriptPrimT
tmap prim =
  case prim of
    TString -> PTextT

vmap :: Value PrimT -> Value PurescriptPrimT
vmap val =
  case val of
    VString t -> PTextV t
