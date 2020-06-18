module UI.ExpenceRow (expenceRow, Message (..), Query (..), Tokens) where

import Prelude

import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Formatter.DateTime (FormatterCommand(..), format) as FD
import Data.Formatter.Number (Formatter(..), format) as FN
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Types (ExpenceWithCommonCurrency, Expence)

data Focus = 
    IdF
    | DateF
    | DescF
    | QtyF
    | AmtF
    | CurrF 
data Query a = Ask (Expence -> a)
data Message = Changed Expence

type Tokens = Hooks.ComponentTokens Query () Message

expenceRow :: forall m. H.Component HH.HTML Query ExpenceWithCommonCurrency Message m
expenceRow = Hooks.component \(tokens :: Tokens) input -> Hooks.do
  date /\ dateId <- Hooks.useState input.date
  description /\ descriptionId <- Hooks.useState input.description
  quantity /\ quantityId <- Hooks.useState input.quantity
  amount /\ amountId <- Hooks.useState input.amount
  currency /\ currencyId <- Hooks.useState input.currency
  commonCurrency /\ commonCurrencyId <- Hooks.useState input.commonCurrency
  exchangeRate /\ exchangeRateId <- Hooks.useState (Nothing :: Maybe Number)
  focus /\ focusId <- Hooks.useState (Nothing :: Maybe Focus)
  let mkExpence :: Expence
      mkExpence = {id: input.id, date, description, quantity, amount, currency}
  Hooks.useQuery tokens.queryToken case _ of
    Ask reply -> pure $ Just $ reply mkExpence
    
  -- Hooks.captures {date, currency, commonCurrency} Hooks.useTickEffect do
  --   liftEffect $ Hooks.modify_
  Hooks.pure $ HH.tr_
    [ HH.td_ [HH.text $ formatDate date]
    , HH.td_ [HH.text description]
    , HH.td_ [HH.text $ formatNumber quantity]
    , HH.td_ [HH.text $ formatNumber amount]
    , HH.td_ [HH.text $ show currency]
    , HH.td_ [HH.text $ formatNumber $ quantity * amount * 2.0]
    ]
  where
    formatDate =
      let formats = fromFoldable 
            [ FD.DayOfMonth
            , FD.Placeholder "."
            , FD.MonthShort
            , FD.Placeholder "."
            , FD.YearFull
            ]
      in FD.format formats <<< toDateTime <<< fromDate
    formatNumber =
      let formatter = FN.Formatter {abbreviations: false, after: 2, before: 0, comma: true, sign: false}
      in FN.format formatter