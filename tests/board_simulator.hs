{-# LANGUAGE
  QuasiQuotes
, TypeFamilies
, GeneralizedNewtypeDeriving
, TemplateHaskell
, OverloadedStrings
, GADTs
, FlexibleContexts #-}


module Main where

import Test.HUnit
import Control.Applicative
import Control.Monad.IO.Class
import Data.Conduit
import Data.IORef
import Data.Time
import Database.Persist.Sql
import Database.Persist.Sqlite
import Market.Board
import Market.Models
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Conduit.List as L

startTime :: UTCTime
startTime = UTCTime
            (fromGregorian 2010 10 10)
            (secondsToDiffTime 0)

stopTime :: UTCTime
stopTime = addUTCTime 3600 startTime -- + one hour


linearTicks :: Rational -> Rational -> Rational -> [Tick]
linearTicks startPrice stopPrice tickCount = map makeTick
                                             $ zip [startPrice,startPrice+step .. stopPrice]
                                             $ iterate (addUTCTime timeStep) startTime
  where
    makeTick (price, time) = Tick "" "" time price 10
    step = (stopPrice - startPrice) / tickCount
    timeStep = realToFrac $ (toRational $ diffUTCTime stopTime startTime) / tickCount

runSim :: Rational -> Rational -> (BoardSimulator -> IO a) -> IO a
runSim startMonay startTickers action = do
  pool <- createSqlitePool ":memory:" 1
  runSqlPersistMPool (runMigration migrateMarketModels) pool
  sim <- createBoardSimulator pool startTime startMonay startTickers
  action sim

insertTicks :: BoardSimulator -> [Tick] -> IO ()

insertTicks sim ticks = do
  let pool = bsPool sim
  (flip runSqlPersistMPool) pool $ do
    mapM_ insert_ ticks

limitSellOrder :: BoardSimulator -> IO ()
limitSellOrder sim = do
  insertTicks sim $ linearTicks 50 150 4000
  registerLimitOrder sim $ LimitOrder startTime Sell 100 1
  simulateUntil sim stopTime
  money <- getMoney sim
  tickers <- getTickers sim
  orders <- listOrders sim
  assertBool "money must be >= 100" $ money >= 100
  assertEqual "tickers mube be 0" tickers 0
  assertEqual "orders must be empty" orders []

limitBuyOrder :: BoardSimulator -> IO ()
limitBuyOrder sim = do
  insertTicks sim $ linearTicks 150 50 4000
  registerLimitOrder sim $ LimitOrder startTime Buy 100 1
  simulateUntil sim stopTime
  money <- getMoney sim
  tickers <- getTickers sim
  orders <- listOrders sim
  assertBool "money must be close to 0" $ money >= 0 && money < 0.1
  assertEqual "tickers mube be 1" tickers 1
  assertEqual "orders must be empty" orders []

mainGroup :: TestTree
mainGroup = testGroup "board simulator"
            [ testCase "sell when grows" $ runSim 0 1 limitSellOrder
            , testCase "buy when lows" $ runSim 100 0 limitBuyOrder]

main :: IO ()
main = defaultMain mainGroup
