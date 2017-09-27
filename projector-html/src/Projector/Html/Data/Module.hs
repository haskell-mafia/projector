{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Data.Module (
    ModuleName (..)
  , moduleNameAppend
  , Module (..)
  , ModuleExpr (..)
  , moduleFree
  , moduleFreeCons
  , moduleBound
  , moduleBoundCons
  , extractModuleBindings
  , extractModuleExprs
  , Imports (..)
  ) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import           P

import           Projector.Core


newtype ModuleName = ModuleName { unModuleName :: Text }
  deriving (Eq, Ord, Show)

moduleNameAppend :: ModuleName -> ModuleName -> ModuleName
moduleNameAppend (ModuleName a) (ModuleName b) =
  ModuleName $
    if T.null b then a else a <> "." <> b

-- TODO might need another datatype, this bakes in a number of
-- assumptions about the backend.
data Module b l a = Module {
    moduleTypes :: TypeDecls l
  , moduleImports :: Map ModuleName Imports
  , moduleExprs :: Map Name (ModuleExpr b l a)
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Ground l => Monoid (Module b l a) where
  mempty = Module mempty mempty mempty
  mappend (Module a b c) (Module d e f) = Module {
      moduleTypes = a <> d
    , moduleImports = b <> e
    , moduleExprs = c <> f
    }

data ModuleExpr b l a = ModuleExpr {
    meParameter :: b
  , meExpr :: Expr l a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | The names of all free variables referenced in this module.
moduleFree :: Module b l a -> Set Name
moduleFree (Module _types _imports exprs) =
  foldl' (flip S.delete) (foldMap (gatherFree . meExpr) exprs) (M.keys exprs)

-- | The names of all the constructors referenced in this module.
moduleFreeCons :: Module b l a -> Set Constructor
moduleFreeCons modl@(Module types _imports exprs) =
  foldl' (flip S.delete) (foldMap (gatherCons . meExpr) exprs <> declFree types) (moduleBoundCons modl)

declFree :: TypeDecls l -> Set Constructor
declFree types =
  S.fromList . join . with (M.toList (unTypeDecls types)) $ \(TypeName _tn, decl) ->
    case decl of
      DVariant cts ->
        foldMap (join . fmap unty . snd) cts
      DRecord fts ->
        foldMap (unty . snd) fts
  where
    -- Awful hacks upon hacks
    unty :: Type l -> [Constructor]
    unty ty =
      case ty of
        TVar (TypeName tn) ->
          [Constructor tn]
        _ ->
          []

-- | Gather all the constructors used in a term.
gatherCons :: Expr l a -> Set Constructor
gatherCons =
  foldlExpr
    (\s expr ->
       case expr of
         ECon _ c _ _ ->
           S.insert c s
         ERec _ (TypeName tn) _ ->
           S.insert (Constructor tn) s
         _ ->
           s)
    (\s pat ->
       case pat of
         PCon _ c _ ->
           S.insert c s
         _ ->
           s)
    S.empty

-- | The names of all the constructors defined in this module.
moduleBoundCons :: Module b l a -> Set Constructor
moduleBoundCons (Module types _imports _exprs) =
  S.fromList . join . with (M.toList (unTypeDecls types)) $ \(TypeName tn, decl) ->
    case decl of
      DVariant cts ->
        -- NOPE none of this will work because of record fields
        --      better off just porting the simple graph code from Machinator
        -- HACKS: better not reuse this typename as a constructor elsewhere
        (Constructor tn) : fmap fst cts
      DRecord _fts ->
        [Constructor tn]

-- | The names of all variables bound/exported in this module.
moduleBound :: Module b l a -> Set Name
moduleBound (Module _types _imports exprs) =
  S.fromList (M.keys exprs)

extractModuleBindings :: Map k (Module b l a) -> Map Name a
extractModuleBindings =
  foldMap (fmap (extractAnnotation . meExpr) . moduleExprs) . M.elems

extractModuleExprs :: Map k (Module b l a) -> Map Name (Expr l a)
extractModuleExprs =
  foldMap (fmap meExpr . moduleExprs) . M.elems

data Imports
  = OpenImport
  | OnlyImport [Name]
  | ImportQualified
  deriving (Eq, Ord, Show)
