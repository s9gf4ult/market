{-# LANGUAGE
  QuasiQuotes
, TypeFamilies
, GeneralizedNewtypeDeriving
, TemplateHaskell
, OverloadedStrings
, GADTs
, FlexibleContexts #-}


module Main where

import Control.Monad(forM_)
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

timeRat :: Rational -> UTCTime
timeRat r | r >= 0 && r <= 1 = addUTCTime t startTime
          | otherwise = error "timeRat: r must be >= 0 && <= 1"
  where
    diff = diffUTCTime stopTime startTime
    t = fromRational $ r * (toRational diff)


linearTicks :: (Money, Money) -> (UTCTime, UTCTime) -> Rational -> [Tick]
linearTicks (startPrice, stopPrice) (begTime, endTime) tickCount = map makeTick
                                                                   $ zip [startPrice,startPrice+step .. stopPrice]
                                                                   $ iterate (addUTCTime timeStep) begTime
  where
    makeTick (price, time) = Tick "" "" time price 10
    step = (stopPrice - startPrice) / (fromRational tickCount)
    timeStep = fromRational $ (toRational $ diffUTCTime endTime begTime) / tickCount

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
  insertTicks sim $ linearTicks (50, 150) (startTime, stopTime) 4000
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
  insertTicks sim $ linearTicks (150, 50) (startTime, stopTime) 4000
  _ <- registerLimitOrder sim $ LimitOrder startTime Buy 100 1
  simulateUntil sim stopTime
  money <- moneyAmount sim
  tickers <- tickersAmount sim
  orders <- listOrders sim
  assertBool "money must be close to 0" $ money >= 0 && money < 0.1
  assertEqual "tickers must be 1" tickers 1
  assertEqual "orders must be empty" orders []

getTheProfit :: (BoardSimulator -> IO ()) -> IO ()
getTheProfit simulateRunner = do
  sim <- makeSim 100 0
  insertTicks sim $ concat
    [linearTicks (100, 40) (startTime, timeRat 0.3) 1000
    ,linearTicks (40, 160) (timeRat 0.3, timeRat 0.8) 2000
    ,linearTicks (160, 100) (timeRat 0.8, stopTime) 1000]
  _ <- registerLimitOrder sim $ LimitOrder startTime Buy 50 1
  _ <- registerLimitOrder sim $ LimitOrder startTime Sell 150 1
  simulateRunner sim
  money <- moneyAmount sim
  tickers <- tickersAmount sim
  orders <- listOrders sim
  assertBool "money > 200" $ money >= 200 && money <= 200.1
  assertEqual "tickers must be 0" tickers 0
  assertEqual "orders must be empty" orders []

simulateInParts :: Int -> BoardSimulator -> IO ()
simulateInParts numparts sim = do
  forM_ parts $ \endTime -> do
    simulateUntil sim endTime
  where
    pts = max 1 numparts
    step = 1 / (toRational pts)
    parts = take pts $ map timeRat $ iterate (+step) step


mainGroup :: TestTree
mainGroup = testGroup "board simulator"
            [ testCase "sell when grows" limitSellOrder
            , testCase "buy when lows" limitBuyOrder
            , testCase "get the profit in 1 parts" $ getTheProfit $ simulateInParts 1
            , testCase "get the profit in 6 parts" $ getTheProfit $ simulateInParts 6
            , testCase "get the profit in 100 parts" $ getTheProfit $ simulateInParts 100]

main :: IO ()
main = defaultMain mainGroup
