import           Disorder.Core.Main

import qualified Test.IO.Projector.Html.Backend.Haskell as Haskell

main :: IO ()
main = do
  disorderMain [
      Haskell.tests
    ]
