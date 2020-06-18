module Main where

import Prelude

import Data.DateTime (Date, canonicalDate)
import Data.Enum (toEnum)
import Data.Maybe (fromJust)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver as HV
import Partial.Unsafe (unsafePartial)
import Types (Currency(..), StoredExpences)
import UI.ExpenceTable as ExpTab

mkDate :: Int -> Int -> Int -> Date
mkDate y m d =
  let yy = toEnum y
      mm = toEnum m
      dd = toEnum d
  in unsafePartial $ fromJust (canonicalDate <$> yy <*> mm <*> dd)
  
exampleData :: StoredExpences
exampleData = 
    { commonCurrency : USD
    , title : "Deneme "
    , expences : 
        [ {id: 1, date: mkDate 2020 1 12, description: "Deneme", quantity: 1.0, amount: 158.0, currency: EUR}

        ]
    }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  HV.runUI ExpTab.expenceTableComponent exampleData body
