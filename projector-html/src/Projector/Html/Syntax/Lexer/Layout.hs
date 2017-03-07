{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Syntax.Lexer.Layout (
    layout
  ) where


import           Control.Monad.Trans.State  (State, runState, gets, modify')

import           Data.DList (DList)
import qualified Data.DList as D

import           P

import           Projector.Html.Data.Position
import           Projector.Html.Syntax.Token


-- | Apply our layout rules, which comprise of
--
-- * Inserting separators and delimiters in place of indentation
-- * Discarding or collapsing whitespace we no longer care about
-- * Throwing errors when the indent is completely wrong
layout :: [Positioned Token] -> [Positioned Token]
layout toks =
  toList . layoutResult . snd . flip runState defaultLayoutState $
    pushLayout HtmlLayout *> applyLayout toks

-- -----------------------------------------------------------------------------

{-

Layout rules:
1. Exprs
     - must continue to the right of where they began
     - should have whitespace collapsed, it never matters
2. Case alternatives
     - must continue to the right of the pattern definition
     - insert ; on layout break
3. Html - hmmmm
     - must continue to the right of the tag open
     - layout whitespace (to the left) should be discarded
     - allow delimited html where we don't do this

-}


data LayoutState = LayoutState {
    layoutMode :: [Layout]
  , layoutResult :: DList (Positioned Token)
  } deriving (Eq, Ord, Show)

defaultLayoutState :: LayoutState
defaultLayoutState =
  LayoutState {
      layoutMode = mempty
    , layoutResult = mempty
    }

pushLayout :: Layout -> State LayoutState ()
pushLayout mode =
  modify' $ \(LayoutState modes result) -> LayoutState (mode : modes) result

popLayout :: State LayoutState ()
popLayout =
  modify' $ \(LayoutState modes result) ->
    case modes of
      (_:xs) ->
        LayoutState xs result
      [] ->
        LayoutState [] result

peekLayout :: State LayoutState (Maybe Layout)
peekLayout =
  fmap head (gets layoutMode)

yieldToken :: Positioned Token -> State LayoutState ()
yieldToken tok =
  modify' $ \(LayoutState modes result) ->
    LayoutState modes (D.snoc result tok)

data Layout =
    ExprLayout
  | HtmlLayout
  | TagOpenLayout
  | TagCloseLayout
  deriving (Eq, Ord, Show)


-- -----------------------------------------------------------------------------

applyLayout :: [Positioned Token] -> State LayoutState ()
applyLayout =
  traverse_ applyLayout'

applyLayout' :: Positioned Token -> State LayoutState ()
applyLayout' tok = do
  mode <- peekLayout
  mcase mode (yieldToken tok) $ \case
    ExprLayout ->
      applyExprLayout tok
    HtmlLayout ->
      applyHtmlLayout tok
    TagOpenLayout ->
      applyTagOpenLayout tok
    TagCloseLayout ->
      applyTagCloseLayout tok

-- - Discard whitespace
-- - Discard newlines
-- - Validate all tokens against some starting point
-- - pop on expr end
applyExprLayout :: Positioned Token -> State LayoutState ()
applyExprLayout tok =
  case tok of
    ExprEnd :@ _ -> do
      popLayout
      yieldToken tok
    Whitespace _ :@ _ ->
      pure ()
    Newline :@ _ ->
      pure ()
    TagOpen :@ _ -> do
      pushLayout TagOpenLayout
      yieldToken tok
    _ ->
      yieldToken tok

-- - dispatch into other layouts
applyHtmlLayout :: Positioned Token -> State LayoutState ()
applyHtmlLayout tok =
  case tok of
    ExprStart :@ _ -> do
      pushLayout ExprLayout
      yieldToken tok
    TagOpen :@ _ -> do
      pushLayout TagOpenLayout
      yieldToken tok
    TagCloseOpen :@ _ -> do
      pushLayout TagCloseLayout
      yieldToken tok
    _ ->
      yieldToken tok

-- - discard whitespace
applyTagOpenLayout :: Positioned Token -> State LayoutState ()
applyTagOpenLayout tok =
  case tok of
    Whitespace _ :@ _ ->
      pure ()
    TagClose :@ _ -> do
      popLayout
      pushLayout HtmlLayout
      yieldToken tok
    TagSelfClose :@ _ -> do
      popLayout
      yieldToken tok
    _ ->
      yieldToken tok

applyTagCloseLayout :: Positioned Token -> State LayoutState ()
applyTagCloseLayout tok =
  case tok of
    Whitespace _ :@ _ ->
      pure ()
    TagClose :@ _ -> do
      popLayout
      yieldToken tok
    _ ->
      yieldToken tok
