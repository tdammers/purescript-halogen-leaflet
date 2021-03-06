module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Leaflet (LEAFLET)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import MainComponent as MC

main :: Eff (HA.HalogenEffects (err :: EXCEPTION, leaflet :: LEAFLET)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI MC.ui unit body
