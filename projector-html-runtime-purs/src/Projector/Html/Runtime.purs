module Projector.Html.Runtime (
    append
  , concat
  , fold
  , isEmpty
  ) where

import Control.Monad as Monad
import Data.String as String
import Prelude ((<>))

append :: String -> String -> String
append =
  (<>)

concat :: Array String -> String
concat =
  String.joinWith ""

fold :: forall a. Array (Array a) -> Array a
fold =
  Monad.join

isEmpty :: forall a. Array a -> Boolean
isEmpty =
  Array.null
