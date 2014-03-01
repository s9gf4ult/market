{-# LANGUAGE
  QuasiQuotes
, TypeFamilies
, GeneralizedNewtypeDeriving
, TemplateHaskell
, OverloadedStrings
, GADTs
, FlexibleContexts #-}


module Market.Board where

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
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Market.Models
import qualified Data.IntMap.Strict as M

newtype OrderId = OrderId Int
                  deriving (Show, Read, Eq, Ord, Bounded, Enum, Integral, Num, Real)

data Direction = Buy | Sell
               deriving (Show, Eq, Ord, Enum, Bounded)

data LimitOrder = LimitOrder
                  { loTime :: UTCTime
                  , loDirection :: Direction
                  , loPrice :: Rational
                  , loVolume :: Rational
                  } deriving (Show, Eq, Ord)

data Order = LO LimitOrder
             deriving (Show, Eq, Ord)

orderTime :: Order -> UTCTime
orderTime (LO lo) = loTime lo

data BoardSimulator = BoardSimulator
                      { bsPool :: ConnectionPool
                      , bsLastTime :: TVar UTCTime
                      , bsOrders :: TVar (M.IntMap Order)
                      , bsMoney :: TVar Rational
                      , bsTickers :: TVar Rational
                      , bsLastOrderId :: TVar OrderId
                      }


createBoardSimulator :: ConnectionPool -> UTCTime -> Rational -> IO BoardSimulator
createBoardSimulator pool lasttime startmoney = atomically $
                                                BoardSimulator
                                                <$> return pool
                                                <*> newTVar lasttime
                                                <*> newTVar M.empty
                                                <*> newTVar startmoney
                                                <*> newTVar 0
                                                <*> newTVar 0


-- Registers new limit order and return it's id
registerLimitOrder :: BoardSimulator -> LimitOrder -> IO OrderId
registerLimitOrder simulator lo = atomically $ do
  oid@(OrderId oidval) <- (+1) <$> (readTVar $ bsLastOrderId simulator)
  modifyTVar' (bsOrders simulator) $ M.insert oidval (LO lo)
  writeTVar (bsLastOrderId simulator) oid
  return oid


-- return True if
cancelOrder :: BoardSimulator -> OrderId -> IO Bool
cancelOrder simulator (OrderId oidval) = atomically $ do
  orders <- readTVar $ bsOrders simulator
  let removed = M.delete oidval orders
  writeTVar (bsOrders simulator) removed
  return $ orders /= removed

listOrders :: BoardSimulator -> IO [(OrderId, Order)]
listOrders simulator = atomically $ do
  orders <- readTVar $ bsOrders simulator
  return $ map (OrderId *** id) $ M.toList orders


-- simulate order execution until specified time
simulateUntil :: BoardSimulator -> UTCTime -> IO ()
simulateUntil simulator uptime = do
  lastT <- readTVarIO $ bsLastTime simulator
  when (uptime > lastT) $ do
    runSqlPersistMPool
      (_simulateUntil simulator uptime)
      $ bsPool simulator
    atomically $ writeTVar (bsLastTime simulator) lastT

_simulateUntil :: (MonadBaseControl IO m, MonadIO m, MonadResource m, MonadLogger m) => BoardSimulator -> UTCTime -> SqlPersistT m ()
_simulateUntil simulator uptime = do
  start <- liftIO $ readTVarIO $ bsLastTime simulator
  selectSource [TickTime >. start, TickTime <=. uptime] [Asc TickTime]
    $$ tickSink simulator

tickSink :: (MonadIO m) => BoardSimulator -> Sink (Entity Tick) m ()
tickSink simulator = awaitForever $ \(Entity _ tick) -> do
  orders <- liftIO $ listOrders simulator
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
            money <- readTVar $ bsMoney simulator
            let needMoney = lVol * tPrice
            when (money >= needMoney) $ do
              writeTVar (bsMoney simulator) $ money - needMoney
              modifyTVar' (bsTickers simulator) (+ lVol)
              modifyTVar' (bsOrders simulator) $ M.delete oid
        Sell -> do
          when (tPrice >= lPrice) $ do
            tickers <- readTVar $ bsTickers simulator
            when (tickers >= lVol) $ do
              writeTVar (bsTickers simulator) $ tickers - lVol
              modifyTVar' (bsMoney simulator) (+ (lVol * tPrice))
              modifyTVar' (bsOrders simulator) $ M.delete oid
