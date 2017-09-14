{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Projector.Html.Backend.Purescript where


import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Disorder.Core
import           Disorder.Jack

import           P

import qualified Projector.Html as Html
import           Projector.Html.Backend.Purescript
import           Projector.Html.Data.Annotation
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim

import           System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

import           Test.IO.Projector.Html.Backend.Property (fileProp, helloWorld, processProp)
import           Test.Projector.Html.Arbitrary


-- -----------------------------------------------------------------------------

prop_empty_module =
  once (moduleProp (ModuleName "Test.Purescript.Module") mempty)

prop_library_module =
  once . modulePropCheck (ModuleName "Test.Purescript.Library") $ Module {
      moduleTypes = mempty
    , moduleImports = mempty
    , moduleExprs = M.fromList [
          helloWorld
        ]
    }

prop_welltyped :: Property
prop_welltyped =
  gamble genHtmlTypeDecls $ \decls ->
    gamble (chooseInt (0,  100)) $ \k ->
      gamble (genWellTypedHtmlModule k decls) $ \modl ->
        moduleProp (ModuleName "Test.Purescript.Arbitrary.WellTyped") $ modl {
            -- TODO once the backend actually does something, remove this setter
            moduleTypes = decls
          , moduleExprs = moduleExprs modl
          }



-- -----------------------------------------------------------------------------

moduleProp :: ModuleName -> Module HtmlType PrimT (HtmlType, a) -> Property
moduleProp mn =
  uncurry pscProp . either (fail . show) id . Html.codeGenModule purescriptBackend mn

modulePropCheck :: ModuleName -> Module (Maybe HtmlType) PrimT SrcAnnotation -> Property
modulePropCheck mn modl@(Module tys _ _) =
  uncurry pscProp . either (fail . T.unpack) id $ do
    modl' <- first Html.renderHtmlError (Html.checkModule tys mempty modl)
    first renderPurescriptError (Html.codeGenModule purescriptBackend mn modl')

pscProp mname modl =
  fileProp mname modl
    (\path ->
      let crpr = (proc "npm" ["run", "-s", "build", "--", path]) { cwd = Just "test/purescript" }
      in readCreateProcessWithExitCode crpr [])
    (processProp (const (property True)))


return []
tests = $disorderCheckEnvAll TestRunFewer
