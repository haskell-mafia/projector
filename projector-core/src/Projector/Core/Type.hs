{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Projector.Core.Type (
  -- * Types
  -- ** Interface
    Type (..)
  , pattern TLit
  , pattern TVar
  , pattern TArrow
  , pattern TList
  , pattern TForall
  , mapGroundType
  -- *** Type functor
  , TypeF (..)
  -- ** Declared types
  , Decl (..)
  , Ground (..)
  , TypeName (..)
  , Constructor (..)
  , FieldName (..)
  , TypeDecls (..)
  , declareType
  , lookupType
  , lookupConstructor
  , subtractTypes
  ) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           P


-- | Types.
newtype Type l = Type (TypeF l (Type l))
  deriving (Eq, Ord, Show)

pattern TLit l = Type (TLitF l)
pattern TVar x = Type (TVarF x)
pattern TArrow a b = Type (TArrowF a b)
pattern TList a = Type (TListF a)
pattern TForall a b = Type (TForallF a b)

-- | Type functor.
data TypeF l a
  = TLitF l
  | TVarF TypeName
  | TArrowF a a
  | TListF a
  | TForallF [TypeName] a
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq l, Eq a) => Eq (TypeF l a)
deriving instance (Ord l, Ord a) => Ord (TypeF l a)
deriving instance (Show l, Show a) => Show (TypeF l a)

-- | Swap out the ground type.
mapGroundType :: Ground l => Ground m => (l -> m) -> Type l -> Type m
mapGroundType tmap (Type ty) =
  Type $ case ty of
    TLitF l ->
      TLitF (tmap l)

    TVarF tn ->
      TVarF tn

    TArrowF a b ->
      TArrowF (mapGroundType tmap a) (mapGroundType tmap b)

    TListF a ->
      TListF (mapGroundType tmap a)

    TForallF as bs ->
      TForallF as (mapGroundType tmap bs)

-- | Declared types.
data Decl l a
  = DVariant [(Constructor, [Type l])] a
  | DRecord [(FieldName, Type l)] a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | The class of user-supplied primitive types.
class (Eq l, Ord l, Show l, Eq (Value l), Ord (Value l), Show (Value l)) => Ground l where
  data Value l
  typeOf :: Value l -> l
  ppGroundType :: l -> Text
  ppGroundValue :: Value l -> Text

-- | A type's name.
newtype TypeName = TypeName { unTypeName :: Text }
  deriving (Eq, Ord, Show)

-- | A constructor's name.
newtype Constructor  = Constructor { unConstructor :: Text }
  deriving (Eq, Ord, Show)

-- | A record field's name.
newtype FieldName = FieldName { unFieldName :: Text }
  deriving (Eq, Ord, Show)

-- | Type contexts.
newtype TypeDecls l a = TypeDecls { unTypeDecls :: Map TypeName (Decl l a) }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Ord l => Monoid (TypeDecls l a) where
  mempty = TypeDecls mempty
  mappend x = TypeDecls . (mappend `on` unTypeDecls) x

declareType :: Ground l => TypeName -> Decl l a -> TypeDecls l a -> TypeDecls l a
declareType n t =
  TypeDecls . M.insert n t . unTypeDecls

lookupType :: Ground l => TypeName -> TypeDecls l a -> Maybe (Decl l a)
lookupType n =
  M.lookup n . unTypeDecls

subtractTypes :: Ground l => TypeDecls l a -> TypeDecls l a -> TypeDecls l a
subtractTypes (TypeDecls m) (TypeDecls n) =
  TypeDecls (M.difference m n)

-- FIX this really sucks, maintain the map in Decls if need be
lookupConstructor :: Ground l => Constructor -> TypeDecls l a -> Maybe (TypeName, [Type l])
lookupConstructor con (TypeDecls m) =
  M.lookup con . M.fromList . mconcat . with (M.toList m) $ \(tn, dec) ->
    case dec of
      DVariant cts _a ->
        with cts $ \(c, ts) ->
          (c, (tn, ts))
      DRecord fts _a ->
        [(Constructor (unTypeName tn), (tn, fmap snd fts))]
