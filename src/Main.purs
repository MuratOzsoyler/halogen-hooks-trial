module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver as HV
import UI.ExpenceTable as ExpTab

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  HV.runUI ExpTab.expenceTableComponent unit body
