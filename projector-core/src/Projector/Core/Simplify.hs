{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Simplify (
    nf
  , whnf
  ) where


import           Projector.Core.Syntax (Expr (..), subst, sextend)


-- | Reduce an expression to weak head normal form, i.e. to the outermost abstraction.
whnf :: Expr l -> Expr l
whnf expr = case expr of
  ELit _ ->
    expr

  EVar _ ->
    expr

  ELam _ _ _ _ ->
    expr

  EApp f g ->
    case whnf f of
      (ELam ss x _ e) ->
        whnf (subst (sextend x g ss) e)

      f' ->
        -- Ill-typed term
        EApp f' g

-- | Reduce an expression to normal form.
nf :: Expr l -> Expr l
nf expr = case expr of
  ELit _ ->
    expr

  EVar _ ->
    expr

  ELam ss n ty e ->
    ELam ss n ty (nf e)

  EApp f g ->
    case whnf f of
      (ELam ss n _ e) ->
        nf (subst (sextend n g ss) e)

      f' ->
        -- Ill-typed term
        EApp (nf f') (nf g)
