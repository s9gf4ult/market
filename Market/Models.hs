{-# LANGUAGE
  QuasiQuotes
, TypeFamilies
, GeneralizedNewtypeDeriving
, TemplateHaskell
, OverloadedStrings
, GADTs
, FlexibleContexts #-}

module Market.Models where

import Database.Persist.TH
import Database.Persist
import Data.Text
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateMarketModels"] [persistLowerCase|
Candle
  board Text
  ticker Text
  period Int
  time UTCTime
  open Rational
  close Rational
  high Rational
  low Rational
  volume Rational

Tick
  board Text
  ticker Text
  time UTCTime
  price Rational
  volume Rational
|]
