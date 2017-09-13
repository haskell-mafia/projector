module Projector.Html.Runtime.Class (
    class DOMLike
  , text
  , textUnescaped
  , parent
  , void
  ) where

import Data.Function.Uncurried (runFn3)
import Projector.Html.Runtime.Class (class DOMLike)
import Pux.Html.Elements as Pux

class DOMLike node attr where
  text :: String -> node
  textUnescaped :: String -> node
  parent :: String -> Array attr -> Array (node) -> node
  void :: String -> Array attr -> node

instance puxDOMLike :: DOMLike (Pux.Html a) (Pux.Attribute a) where
  text =
    Pux.text
  textUnescaped =
    Pux.text -- FIXME?
  parent name =
    runFn3 Pux.element name
  void name attrs =
    runFn3 Pux.element name attrs []
