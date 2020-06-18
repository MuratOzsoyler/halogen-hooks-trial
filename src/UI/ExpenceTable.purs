module UI.ExpenceTable
        ( expenceTableComponent
        ) where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Types (Currency (..))
import UI.CurrencySelectComponent as Curr

_commonCurrLabel :: SProxy "commonCurr"
_commonCurrLabel = SProxy

expenceTableColHdrs :: Currency -> Array String
expenceTableColHdrs curr = 
  ["Tarih", "Açıklama", "Miktar", "Tutar", "Toplam", show curr <> " karşılığı"]

expenceTableComponent 
  :: forall query input output m
   . MonadAff m
  => H.Component HH.HTML query input output m
expenceTableComponent = Hooks.component \_ _ -> Hooks.do
  commonCurr /\ commonCurrId <- Hooks.useState TRL

  let commonCurrencyHandler (Curr.Changed curr) = Just do
        liftEffect $ log $ "Heyooo " <> show curr 
        liftEffect <<< log <<< ("State modified " <> _) <<< show 
          =<< Hooks.modify commonCurrId (const curr)

  Hooks.pure $ HH.table_ [ HH.thead_
    [ HH.tr_ [ HH.th [HP.colSpan $ length $ expenceTableColHdrs commonCurr]
        [ HH.text "Ortak para birimi:"
        , HH.slot _commonCurrLabel 0 Curr.currencySelectComponent commonCurr commonCurrencyHandler
        ]]
    , HH.tr_ $ map (\h -> HH.th_ [HH.text h]) $ expenceTableColHdrs commonCurr
    ]]
