{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Projector.Core.Syntax (
    Expr (..)
  , Name (..)
  -- * Substitution sets
  , Subs (..)
  , subst
  , sextend
  , slookup
  , sempty
  , sunion
  -- * Smart/lazy constructors
  , lam
  , lam_
  , var_
  ) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           P

import           Projector.Core.Type (Type (..), Ground (..))


-- | The type of Projector expressions.
--
-- The first type parameter, 'l', refers to the type of literal. This is
-- invariant. Literals must have a 'Ground' instance.
data Expr l
  = ELit (Value l)
  | EVar Name
  | ELam (Subs l) Name (Type l) (Expr l)
  | EApp (Expr l) (Expr l)

deriving instance (Eq l, Eq (Value l)) => Eq (Expr l)
deriving instance (Show l, Show (Value l)) => Show (Expr l)
deriving instance (Ord l, Ord (Value l)) => Ord (Expr l)

newtype Name = Name { unName :: Text }
  deriving (Eq, Ord, Show)


-- -----------------------------------------------------------------------------
-- The type of substitutions.

newtype Subs l = Subs { unSubs :: Map Name (Expr l) }

deriving instance (Eq l, Eq (Value l)) => Eq (Subs l)
deriving instance (Show l, Show (Value l)) => Show (Subs l)
deriving instance (Ord l, Ord (Value l)) => Ord (Subs l)

instance Monoid (Subs l) where
  mempty = Subs mempty
  mappend (Subs a) (Subs b) = Subs (mappend a b)

subst :: Subs l -> Expr l -> Expr l
subst ss0 expr =
  case expr of
    ELit _ ->
      expr

    EVar x ->
      fromMaybe expr (slookup x ss0)

    ELam ss1 x t e ->
      let ss2 = smap (subst ss0) ss1
      in ELam (sunion ss2 ss0) x t e

    EApp f g ->
      EApp (subst ss0 f) (subst ss0 g)

sempty :: Subs l
sempty =
  Subs mempty

sextend :: Name -> Expr l -> Subs l -> Subs l
sextend n e =
  Subs . M.insert n e . unSubs

slookup :: Name -> Subs l -> Maybe (Expr l)
slookup n =
  M.lookup n . unSubs

sunion :: Subs l -> Subs l -> Subs l
sunion a b =
  a <> b

smap :: (Expr l -> Expr l) -> Subs l -> Subs l
smap f =
  Subs . fmap f . unSubs

-- -----------------------------------------------------------------------------

lam :: Name -> Type l -> Expr l -> Expr l
lam n ty =
  ELam sempty n ty

-- lazy
lam_ :: Text -> Type l -> Expr l -> Expr l
lam_ n =
  lam (Name n)

var_ :: Text -> Expr l
var_ =
  EVar . Name
