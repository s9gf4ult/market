{-# LANGUAGE
  QuasiQuotes
, TypeFamilies
, GeneralizedNewtypeDeriving
, TemplateHaskell
, OverloadedStrings
, GADTs
, FlexibleContexts #-}

module Market.Models where

import Data.Text
import Data.Time
import Database.Persist.TH
import Market.Models.Fields



share [mkPersist sqlSettings, mkMigrate "migrateMarketModels"] [persistLowerCase|
Candle
  board Text
  ticker Text
  period Int
  time UTCTime
  open Money
  close Money
  high Money
  low Money
  volume Ticker
  deriving Show Eq

Tick
  board Text
  ticker Text
  time UTCTime
  price Money
  volume Ticker
  deriving Show Eq

Session
  client Text
  deriving Show Eq

Transaction
  board Text
  ticker Text
  sessionId SessionId
  direction Direction
  time UTCTime
  price Money
  volume Ticker
  deriving Show Eq
|]
