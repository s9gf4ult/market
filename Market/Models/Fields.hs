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
import qualified Data.Text as T

data Direction = Buy | Sell
               deriving (Show, Eq, Ord, Enum, Bounded)

instance  PersistField Direction where
  toPersistValue Buy = PersistInt64 0
  toPersistValue Sell = PersistInt64 1
  fromPersistValue (PersistInt64 0) = Right Buy
  fromPersistValue (PersistInt64 1) = Right Sell
  fromPersistValue (PersistText "buy") = Right Buy
  fromPersistValue (PersistText "sell") = Right Sell
  fromPersistValue x = Left $ T.pack $ "could not convert value "
                       ++ (show x) ++ " to Direction"

instance PersistFieldSql Direction where
  sqlType _ = SqlInt32

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
