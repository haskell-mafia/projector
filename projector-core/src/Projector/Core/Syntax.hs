{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Projector.Core.Syntax (
    Expr (..)
  , lam
  ) where


import           Bound (Scope (..),  (>>>=))
import           Bound.Name (Name (..), abstract1Name)

import           P

import           Prelude.Extras (Eq1, Ord1, Show1)

import           Projector.Core.Type (Type (..), Ground (..))


-- | The type of Projector expressions.
--
-- This uses the Bound style, where an AST is parameterised by the type of names.
-- The Functor, Applicative, Monad, Foldable and Traversable instances operate
-- on all the free variables in the syntax tree.
--
-- The first type parameter, 'l', refers to the type of literal. This is
-- invariant. Literals must have a 'Ground' instance.
--
-- The second parameter, 'n', refers to the original type of name, in the
-- source code. It's carried around for error reporting. This is invariant.
--
-- The third parameter, 'a', refers to the current type of name, in
-- the Bound style. 'Expr' is covariant in 'a'.
data Expr l n a
  = ELit (Value l)
  | EVar a
  | ELam (Type l) (Scope (Name n ()) (Expr l n) a)
  | EApp (Expr l n a) (Expr l n a)
  deriving (Functor, Foldable, Traversable)

-- Default instances for various functor classes (Eq1, Show1, etc)
--
-- These are also defined in Data.Functor.Classes in transformers,
-- but only in newer versions >= 5.0. So, let's use the versions in
-- prelude-extras, which is already a transitive dependency.
--
-- We need these to achieve 'Eq' and 'Show' on 'Expr'.
-- 'Eq' is alpha equivalence, thanks to Bound.
-- 'Show' does not respect alpha equivalence.
instance (Eq l, Eq (Value l), Eq n) => Eq1 (Expr l n)
deriving instance (Eq l, Eq (Value l), Eq n, Eq a) => Eq (Expr l n a)

instance (Ord l, Ord (Value l), Ord n) => Ord1 (Expr l n)
deriving instance (Ord l, Ord (Value l), Ord n, Ord a) => Ord (Expr l n a)

instance (Show l, Show (Value l), Show n) => Show1 (Expr l n)
deriving instance (Show l, Show (Value l), Show n, Show a) => Show (Expr l n a)


instance Applicative (Expr l n) where
  pure = EVar
  (<*>) = ap

instance Monad (Expr l n) where
  return = pure
  a >>= f = case a of
    ELit l ->
      ELit l

    EVar x ->
      f x

    ELam t x ->
      -- (>>>=) lifts bind operations over the Scope monad transformer (and family)
      -- (>>>=) :: (Monad f, Bound t) => t f a -> (a -> f c) -> t f c
      -- (>>>=) :: Scope () Expr a -> (a -> Expr c) -> Scope () Expr c
      -- x :: Scope () Expr a, f :: a -> Expr b
      -- i.e. it lets us walk under binders and apply f to all the free variables in the body.
      ELam t (x >>>= f )

    EApp x y ->
      EApp (x >>= f) (y >>= f)


-- | Construct a lambda abstraction from a name, a type, and an expression.
lam :: Eq a => a -> Type l -> Expr l a a -> Expr l a a
lam v t b =
  -- abstract1Name constructs a scope from a name and an expr
  -- abstract1Name :: (Monad f, Eq a) => a -> f a -> Scope (Name a ()) f a
  -- abstract1Name :: a -> Expr l a -> Scope (Name a ()) (Expr l) a
  ELam t (abstract1Name v b)