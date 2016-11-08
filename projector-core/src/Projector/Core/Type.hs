{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Projector.Core.Type (
    Type (..)
  , Ground (..)
  , TypeName (..)
  , Constructor (..)
  , RecType (..)
  , fromRecType
  ) where


import           P


-- | Types.
data Type l
  = TLit l
  | TArrow (Type l) (Type l)
  | TVariant TypeName [(Constructor, [RecType l])]
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

data RecType l
  = RThis
  | RType (Type l)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

-- | The class of user-supplied primitive types.
class Eq l => Ground l where
  data Value l
  typeOf :: Value l -> l
  ppGroundType :: l -> Text
  ppGroundValue :: Value l -> Text

-- | A type's name.
newtype TypeName = TypeName { unTypeName :: Text }
  deriving (Eq, Ord, Show, Read)

-- | A constructor's name.
newtype Constructor  = Constructor { unConName :: Text }
  deriving (Eq, Ord, Show, Read)

-- | Instantiate a recursive type. Isomorphic to 'fromMaybe'.
fromRecType :: Type l -> RecType l -> Type l
fromRecType ty r = case r of
  RThis ->
    ty
  RType t2 ->
    t2
