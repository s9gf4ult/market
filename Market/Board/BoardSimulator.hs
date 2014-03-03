{-# LANGUAGE
  QuasiQuotes
, TypeFamilies
, GeneralizedNewtypeDeriving
, TemplateHaskell
, OverloadedStrings
, GADTs
, FlexibleContexts #-}

module Market.Board.BoardSimulator where

import Market.Board.Types

import Control.Applicative
import Control.Arrow
import Control.Concurrent.STM.TVar
import Control.Monad (when, forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.STM
import Data.Conduit
import Data.Function (on)
import Data.List (sortBy)
import Data.Text(Text)
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Market.Models
import qualified Data.IntMap.Strict as M


data BoardSimulator = BoardSimulator
                      { bsName :: Text
                      , bsTickerName :: Text
                      , bsMoneyName :: Text
                      , bsPool :: ConnectionPool
                      , bsLastTime :: TVar UTCTime
                      , bsOrders :: TVar (M.IntMap Order)
                      , bsMoney :: TVar Money
                      , bsTickers :: TVar Ticker
                      , bsLastOrderId :: TVar OrderId
                      }


createBoardSimulator :: Text
                        -> Text
                        -> Text
                        -> ConnectionPool
                        -> UTCTime
                        -> Money
                        -> Ticker
                        -> IO BoardSimulator
createBoardSimulator
  boardName
  tickerName
  moneyName
  pool
  lasttime
  startmoney
  starttickers = atomically
                 $ BoardSimulator
                 <$> return boardName
                 <*> return tickerName
                 <*> return moneyName
                 <*> return pool
                 <*> newTVar lasttime
                 <*> newTVar M.empty
                 <*> newTVar startmoney
                 <*> newTVar starttickers
                 <*> newTVar 0


instance Board BoardSimulator where
  boardName board = bsName board
  boardMoneyName board = bsMoneyName board
  boardTickerName board = bsTickerName board

  listOrders board = atomically $ do
    orders <- readTVar $ bsOrders board
    return $ map (OrderId *** id) $ M.toList orders

  cancelOrder simulator (OrderId oidval) = atomically $ do
    orders <- readTVar $ bsOrders simulator
    let removed = M.delete oidval orders
    writeTVar (bsOrders simulator) removed
    return $ orders /= removed

  moneyAmount board = readTVarIO $ bsMoney board
  tickersAmount board = readTVarIO $ bsTickers board

instance CanLimitOrder BoardSimulator where

  registerLimitOrder simulator lo = atomically $ do
    oid@(OrderId oidval) <- (+1) <$> (readTVar $ bsLastOrderId simulator)
    modifyTVar' (bsOrders simulator) $ M.insert oidval (LO lo)
    writeTVar (bsLastOrderId simulator) oid
    return oid


-- simulate order execution until specified time
simulateUntil :: BoardSimulator -> UTCTime -> IO ()
simulateUntil simulator uptime = do
  fromtime <- readTVarIO $ bsLastTime simulator
  when (uptime > fromtime) $ do
    runSqlPersistMPool
      (_simulateUntil simulator fromtime uptime)
      $ bsPool simulator

_simulateUntil :: (MonadBaseControl IO m, MonadIO m, MonadResource m, MonadLogger m) =>
                  BoardSimulator
                  -> UTCTime
                  -> UTCTime
                  -> SqlPersistT m ()
_simulateUntil simulator fromtime uptime = do
  selectSource [ TickTime >. fromtime
               , TickTime <=. uptime
               , TickBoard ==. (boardName simulator)
               , TickTicker ==. (boardTickerName simulator)
               ] [Asc TickTime]
    $$ tickSink simulator

tickSink :: (MonadIO m) => BoardSimulator -> Sink (Entity Tick) m ()
tickSink sim = awaitForever $ \(Entity _ tick) -> do
  orders <- liftIO $ listOrders sim
  let tt = tickTime tick
      actual_orders = filter (\(_, o) -> orderTime o <= tt) orders
      sorted_orders = sortBy (compare `on` orderTime . snd) actual_orders
  forM_ sorted_orders $ tryExecuteOrder tick
  where
    tryExecuteOrder tick ((OrderId oid), (LO limitOrder)) = do
      let tPrice = tickPrice tick
          lPrice = loPrice limitOrder
          lVol = loVolume limitOrder
      liftIO $ atomically $ case loDirection limitOrder of
        Buy -> do
          when (tPrice <= lPrice) $ do
            money <- readTVar $ bsMoney sim
            let needMoney = lVol * tPrice
            when (money >= needMoney) $ do
              writeTVar (bsMoney sim) $ money - needMoney
              modifyTVar' (bsTickers sim) (+ lVol)
              modifyTVar' (bsOrders sim) $ M.delete oid
              writeTVar (bsLastTime sim)
        Sell -> do
          when (tPrice >= lPrice) $ do
            tickers <- readTVar $ bsTickers sim
            when (tickers >= lVol) $ do
              writeTVar (bsTickers sim) $ tickers - lVol
              modifyTVar' (bsMoney sim) (+ (lVol * tPrice))
              modifyTVar' (bsOrders sim) $ M.delete oid
