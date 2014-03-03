{-# LANGUAGE
  QuasiQuotes
, TypeFamilies
, GeneralizedNewtypeDeriving
, TemplateHaskell
, OverloadedStrings
, GADTs
, FlexibleContexts #-}


module Market.Models.Fields where

import Database.Persist
import Database.Persist.Sql

newtype Money = Money {unMoney :: Rational}
                deriving (Enum, Eq, Fractional,
                          Num, Ord, Read, Real,
                          RealFrac, Show,
                          PersistField, PersistFieldSql)

newtype Ticker = Ticker {unTicker :: Rational}
                deriving (Enum, Eq, Fractional,
                          Num, Ord, Read, Real,
                          RealFrac, Show,
                          PersistField, PersistFieldSql)
