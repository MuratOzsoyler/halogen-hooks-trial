module Types 
        ( CommonCurrencyRep
        , Currency (..)
        , Expence
        , ExpenceRep
        , ExpencesRep
        , ExpenceWithCommonCurrency
        , StoredExpences
        ) where

import Prelude

import Data.Date (Date)
import Data.Enum (class Enum)
import Data.Maybe (Maybe(..))
import Data.String.Read (class Read)
import Type.Row (type (+))

data Currency = TRL | USD | EUR

derive instance currencyEq   :: Eq   Currency 
derive instance currencyOrd   :: Ord   Currency 
instance currencyShow :: Show Currency where
  show TRL = "TRL" 
  show USD = "USD" 
  show EUR = "EUR" 
instance currencyRead :: Read Currency where
  read "TRL" = Just TRL
  read "USD" = Just USD
  read "EUR" = Just EUR
  read _ = Nothing
instance currencyEnum :: Enum Currency where
  pred TRL = Nothing
  pred USD = Just TRL
  pred EUR = Just USD
  succ TRL = Just USD 
  succ USD = Just EUR
  succ EUR = Nothing

type CommonCurrencyRep row = (commonCurrency :: Currency | row)

type ExpenceRep row =
        ( id :: Int
        , date :: Date
        , description :: String
        , quantity :: Number
        , currency :: Currency
        , amount :: Number
        | row
        )

type Expence = { | ExpenceRep () }
type ExpenceWithCommonCurrency = { | CommonCurrencyRep + ExpenceRep () }

type ExpencesRep row = 
        ( title :: String
        , expences :: Array Expence 
        | row
        )

type StoredExpences = { | CommonCurrencyRep + ExpencesRep () }

