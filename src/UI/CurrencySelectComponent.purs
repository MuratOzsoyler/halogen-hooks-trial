module UI.CurrencySelectComponent
        ( currencySelectComponent
        , Message (..)
        , Query (..)
        , Slot
        , Tokens
        ) where

import Prelude

import Data.Enum (enumFromTo)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String.Read (read)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Partial.Unsafe (unsafePartial)
import Types (Currency (..))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML as HTML
import Web.HTML.HTMLSelectElement as Select

data Query a = WhatIs (Currency -> a)

data Message = Changed Currency

type Slot = H.Slot Query Message

type Tokens = Hooks.ComponentTokens Query () Message

currencySelectComponent
  :: forall m
   . MonadAff m
  => H.Component HH.HTML Query Currency Message m
currencySelectComponent = Hooks.component \(tokens :: Tokens) (input :: Currency) -> Hooks.do
  currency /\ currencyId <- Hooks.useState input
  Hooks.useQuery tokens.queryToken case _ of
      WhatIs reply  -> pure $ Just $ reply currency
  let id = H.RefLabel "currSelect"
      isSelected curr = curr == currency
      opt c = HH.option [HP.selected $ isSelected c] [HH.text $ show c]
      handleChange =
          Hooks.getHTMLElementRef id
          >>= unsafePartial fromJust >>> toSelectElement >>> pure
          >>= Select.value >>> liftEffect
          >>= read >>> fromMaybe TRL >>> const >>> Hooks.modify currencyId
          >>= Changed >>> Hooks.raise tokens.outputToken
  Hooks.pure $ HH.select
      [ HP.ref id
      , HE.onChange \_ -> Just handleChange
      ]
      (map opt $ enumFromTo TRL EUR)
  where
    toSelectElement :: HTML.HTMLElement -> Select.HTMLSelectElement
    toSelectElement = unsafeCoerce