{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Projector.Html.Backend.Purescript.Rewrite (
    rewriteModule
  , rewriteExpr
  , rules
  ) where


import           P

import           Projector.Core
import qualified Projector.Core as Core
import qualified Projector.Html.Backend.Rewrite as Rewrite
import qualified Projector.Html.Core.Library as CL
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim


rewriteModule :: ModuleName -> Module HtmlType PrimT a -> (ModuleName, Module HtmlType PrimT a)
rewriteModule mn (Module tys imports exprs) =
  let exprs' = fmap (\(ModuleExpr ty e) -> ModuleExpr ty (rewriteFix (Rewrite.globalRules <> rules) e)) exprs
  in (mn, Module tys imports exprs')

rewriteExpr :: Expr PrimT a -> Expr PrimT a
rewriteExpr =
  Core.rewrite (Rewrite.globalRules <> rules)

-- * Drop all comments, they don't make sense in virtual-dom world
-- * Replace constructors with DOMLike typeclass methods
rules :: [RewriteRule PrimT a]
rules =
  fmap Rewrite [
      -- Replace HTML model with DOMLike functions.
      -- These rules are important for correctness - won't work without these.
      (\case ECon a (Constructor "Plain") _ [x] ->
               pure (apply (textNode a) [x])
             _ ->
               empty)
    , (\case ECon a (Constructor "Raw") _ [x] ->
               pure (apply (rawTextNode a) [x])
             _ ->
               empty)
    , (\case ECon a (Constructor "Element") _ [tag, attrs, body] ->
               pure (apply (parentNode a) [tag, attrs, body])
             _ ->
               empty)
    , (\case ECon a (Constructor "VoidElement") _ [tag, attrs] ->
               pure (apply (voidNode a) [tag, attrs])
             _ ->
               empty)
{--
    , (\case ECon a (Constructor "Comment") _ [str] ->
               pure (apply (comment a) [str])
             _ ->
               empty)
--}
{--
    , (\case ECon a (Constructor "Nested") _ [nodes] ->
               pure (apply (foldHtml a) [nodes])
             _ ->
               empty)
--}

      -- Qualify imports for runtime functions and constructors.
    , (\case EForeign a (Name "append") ty ->
               pure (EForeign a (Name "Projector.Html.Runtime.append") ty)
             _ ->
               empty)
    , (\case EForeign a (Name "concat") ty ->
               pure (EForeign a (Name "Projector.Html.Runtime.concat") ty)
             _ ->
               empty)
    , (\case EForeign a (Name "fold") ty ->
               pure (EForeign a (Name "Projector.Html.Runtime.fold") ty)
             _ ->
               empty)
    , (\case EForeign a (Name "isEmpty") ty ->
               pure (EForeign a (Name "Projector.Html.Runtime.isEmpty") ty)
             _ ->
               empty)
      -- TODO consider copying constructor qualifying code across
    ]


-- build an application chain
apply :: Expr PrimT a -> [Expr PrimT a] -> Expr PrimT a
apply f =
  foldl' (EApp (extractAnnotation f)) f

textNode :: a -> Expr PrimT a
textNode a =
  EForeign a (Name "Projector.Html.Runtime.text") (TArrow (TLit TString) CL.tHtml)

rawTextNode :: a -> Expr PrimT a
rawTextNode a =
  EForeign a (Name "Projector.Html.Runtime.textUnescaped") (TArrow (TLit TString) CL.tHtml)

parentNode :: a -> Expr PrimT a
parentNode a =
  EForeign a (Name "Projector.Html.Runtime.parent") (TArrow CL.tTag (TArrow (TList CL.tAttribute) (TArrow CL.tHtml CL.tHtml)))

voidNode :: a -> Expr PrimT a
voidNode a =
  EForeign a (Name "Projector.Html.Runtime.void") (TArrow CL.tTag (TArrow (TList CL.tAttribute) CL.tHtml))
