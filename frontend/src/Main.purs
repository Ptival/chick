module Main where

import Prelude
--import Halogen as H
import Halogen.Aff as HA
import Chick.Component as Chick
import Control.Monad.Aff (delay)
import Control.Monad.Eff (Eff)
--import Control.Monad.Eff.Console (log)
import Control.Monad.Rec.Class (forever)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Halogen.Aff.Util (selectElement)
import Halogen.VDom.Driver (runUI)

pingDelay :: Milliseconds
pingDelay = Milliseconds 1000.0

main :: ∀ e. Eff (HA.HalogenEffects (Chick.ChickEffects e)) Unit
main = HA.runHalogenAff do
  selectElement (QuerySelector "body") >>= traverse_ \ body -> do
    chick <- runUI Chick.chickComponent unit body
    _ <- forever $ do
      --chick.query $ H.action Chick.SAPIPing
      delay pingDelay
    pure unit
