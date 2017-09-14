module Projector.Html.Runtime.Pux (
    text
  , textUnescaped
  , parent
  , void
  , fold
  , blank
  , attr
  ) where

import Data.Array as Array
import Data.Function.Uncurried (runFn3)
import Prelude
import Pux.Html.Attributes (attr) as Pux
import Pux.Html.Elements (Html, Attribute, element, text) as Pux


-- Pux 6 Html doesn't have a valid fold/concat, so we resort to this
-- awful hack: Everything must be an array.
--
-- The array concatenation here will lead to remarkably poor
-- performance. We just have to live with this until we can upgrade
-- to the newer version of Pux, or something else based on Smolder.
type Html ev = Array (Pux.Html ev)

text :: forall ev. String -> Html ev
text =
  Array.singleton <<< Pux.text

textUnescaped :: forall ev. String -> Html ev
textUnescaped =
  Array.singleton <<< Pux.text

parent :: forall ev. String -> Array (Pux.Attribute ev) -> Array (Html ev) -> Html ev
parent name attrs =
  Array.singleton <<< runFn3 Pux.element name attrs <<< Array.concat

void :: forall ev. String -> Array (Pux.Attribute ev) -> Html ev
void name attrs =
  Array.singleton (runFn3 Pux.element name attrs [])

fold :: forall ev. Array (Html ev) -> Html ev
fold =
  Array.concat

blank :: forall ev. Html ev
blank =
  []

attr :: forall ev. String -> String -> Pux.Attribute ev
attr =
  Pux.attr
