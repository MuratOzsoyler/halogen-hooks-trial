module UI.ExpenceTable
        ( expenceTableComponent
        ) where

import Prelude

import Data.Array (findIndex, length, modifyAt)
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
import Types (Currency, StoredExpences, Expence)
import UI.CurrencySelectComponent as Curr
import UI.ExpenceRow as ExpRow
import Web.HTML.Event.EventTypes (offline)

_commonCurrLabel :: SProxy "_commonCurr"
_commonCurrLabel = SProxy

_expenceRowLabel :: SProxy "_expenceRow"
_expenceRowLabel = SProxy

expenceTableColHdrs :: Currency -> Array String
expenceTableColHdrs curr = 
  ["Tarih", "Açıklama", "Miktar", "Tutar", "Toplam", show curr <> " karşılığı"]

expenceTableComponent 
  :: forall query output m
   . MonadAff m
  => H.Component HH.HTML query StoredExpences output m
expenceTableComponent = Hooks.component \_ input -> Hooks.do
  commonCurr /\ commonCurrId <- Hooks.useState input.commonCurrency
  expences /\ expencesId <- Hooks.useState input.expences

  let colSpan = length $ expenceTableColHdrs commonCurr
      commonCurrencyHandler (Curr.Changed curr) = Just do
        liftEffect $ log $ "Heyooo " <> show curr 
        liftEffect <<< log <<< ("State modified " <> _) <<< show 
          =<< Hooks.modify commonCurrId (const curr)

  let mkExpenceWComCurr {id , date , description , quantity , currency , amount} = 
        { commonCurrency : commonCurr
        , id
        , date
        , description
        , quantity
        , amount
        , currency
        }
      handleExpence :: ExpRow.Message -> Maybe (Hooks.HookM m Unit )
      handleExpence (ExpRow.Changed e) = Just do 
        let newExpences :: Maybe (Array Expence)
            newExpences = 
                findIndex (\x -> x.id == e.id) expences
                >>= (\idx -> modifyAt idx (const e) expences)
        -- pure unit
        case newExpences of
          Nothing -> pure unit
          Just exps -> Hooks.put expencesId exps 
      row exp = HH.slot _expenceRowLabel exp.id ExpRow.expenceRow (mkExpenceWComCurr exp) handleExpence

  Hooks.pure $ HH.table_ 
    [ HH.thead_
        [ HH.tr_ [ HH.th [ HP.colSpan colSpan ] [ HH.h1_ [ HH.text input.title ]]] -- $ length $ expenceTableColHdrs commonCurr]
        , HH.tr_ [ HH.th [ HP.colSpan colSpan ] -- $ length $ expenceTableColHdrs commonCurr]
            [ HH.text "Ortak para birimi:"
            , HH.slot _commonCurrLabel 0 Curr.currencySelectComponent commonCurr commonCurrencyHandler
            ]]
        , HH.tr_ $ map (\h -> HH.th_ [HH.text h]) $ expenceTableColHdrs commonCurr
        ]
    , HH.tbody_ $ map row expences 
    ]
