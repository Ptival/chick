module Main where

import Prelude
import Chick.Component as Chick
import Halogen as H
import Halogen.Aff as HA
import Halogen.Aff.Effects as HAE
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Traversable (traverse_)
import Halogen.VDom.Driver (runUI)

type ChickEffects e =
  ( console :: CONSOLE
  | e)

main :: ∀ e. Eff (HAE.HalogenEffects (ChickEffects e)) Unit
main = HA.runHalogenAff do
  HA.selectElement (QuerySelector "body") >>= traverse_ \ body -> do
    H.liftAff $ log $ "Chick is live!"
    chick <- runUI Chick.chickComponent unit body
    pure unit
