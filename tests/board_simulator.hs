{-# LANGUAGE
  QuasiQuotes
, TypeFamilies
, GeneralizedNewtypeDeriving
, TemplateHaskell
, OverloadedStrings
, GADTs
, FlexibleContexts #-}


module Main where

import Data.Time
import Database.Persist.Sql
import Database.Persist.Sqlite
import Market.Board.BoardSimulator
import Market.Board.Types
import Market.Models
import Market.Models.Fields
import Test.HUnit
import Test.Tasty
import Test.Tasty.HUnit

startTime :: UTCTime
startTime = UTCTime
            (fromGregorian 2010 10 10)
            (secondsToDiffTime 0)

stopTime :: UTCTime
stopTime = addUTCTime 3600 startTime -- + one hour


linearTicks :: Money -> Money -> Rational -> [Tick]
linearTicks startPrice stopPrice tickCount = map makeTick
                                             $ zip [startPrice,startPrice+step .. stopPrice]
                                             $ iterate (addUTCTime timeStep) startTime
  where
    makeTick (price, time) = Tick "" "" time price 10
    step = (stopPrice - startPrice) / (fromRational tickCount)
    timeStep = fromRational $ (toRational $ diffUTCTime stopTime startTime) / tickCount

makeSim :: Money -> Ticker -> IO BoardSimulator
makeSim startMoney startTickers = do
  pool <- createSqlitePool ":memory:" 1
  runSqlPersistMPool (runMigration migrateMarketModels) pool
  createBoardSimulator "" "" "" pool startTime startMoney startTickers

insertTicks :: BoardSimulator -> [Tick] -> IO ()
insertTicks sim ticks = do
  let pool = bsPool sim
  (flip runSqlPersistMPool) pool $ do
    mapM_ insert_ ticks

limitSellOrder :: IO ()
limitSellOrder = do
  sim <- makeSim 0 1
  insertTicks sim $ linearTicks 50 150 4000
  _ <- registerLimitOrder sim $ LimitOrder startTime Sell 100 1
  simulateUntil sim stopTime
  money <- moneyAmount sim
  tickers <- tickersAmount sim
  orders <- listOrders sim
  assertBool "money must be >= 100 && < 100.1" $ money >= 100 && money < 100.1
  assertEqual "tickers mube be 0" tickers 0
  assertEqual "orders must be empty" orders []

limitBuyOrder :: IO ()
limitBuyOrder = do
  sim <- makeSim 100 0
  insertTicks sim $ linearTicks 150 50 4000
  _ <- registerLimitOrder sim $ LimitOrder startTime Buy 100 1
  simulateUntil sim stopTime
  money <- moneyAmount sim
  tickers <- tickersAmount sim
  orders <- listOrders sim
  assertBool "money must be close to 0" $ money >= 0 && money < 0.1
  assertEqual "tickers mube be 1" tickers 1
  assertEqual "orders must be empty" orders []

mainGroup :: TestTree
mainGroup = testGroup "board simulator"
            [ testCase "sell when grows" $ limitSellOrder
            , testCase "buy when lows" $ limitBuyOrder]

main :: IO ()
main = defaultMain mainGroup
