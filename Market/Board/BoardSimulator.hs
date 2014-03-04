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
import Control.Monad ( when, forM_ )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Logger ( MonadLogger )
import Control.Monad.STM
import Control.Monad.Trans.Class( MonadTrans(..) )
import Data.Conduit
import Data.Function (on)
import Data.List (sortBy)
import Data.Text(Text)
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Market.Models
import Market.Models.Fields
import qualified Data.IntMap.Strict as M


data BoardSimulator = BoardSimulator
                      { bsName :: Text
                      , bsTickerName :: Text
                      , bsMoneyName :: Text
                      , bsPool :: ConnectionPool
                      , bsSession :: SessionId
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
  board
  tickerName
  moneyName
  pool
  lasttime
  startmoney
  starttickers = do
    sid <- (flip runSqlPersistMPool) pool $ do
      insert $ Session "BoardSimulator"
    atomically
      $ BoardSimulator
      <$> return board
      <*> return tickerName
      <*> return moneyName
      <*> return pool
      <*> return sid
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

instance HasHistory BoardSimulator where
  lastTransactions sim cnt = do
    let p = bsPool sim
        sid = bsSession sim
    (flip runSqlPersistMPool) p $ do
      (map entityVal) <$> selectList
        [TransactionSessionId ==. sid
        ,TransactionBoard ==. boardName sim
        ,TransactionTicker ==. boardTickerName sim]
        [Desc TransactionTime, LimitTo cnt]

runWithPool :: BoardSimulator -> SqlPersistM a -> IO a
runWithPool sim action = do
  let p = bsPool sim
  runSqlPersistMPool action p

-- simulate order execution until specified time
simulateUntil :: (MonadBaseControl IO m, MonadIO m, MonadResource m, MonadLogger m) =>
                 BoardSimulator -> UTCTime -> SqlPersistT m ()
simulateUntil simulator uptime = do
  fromtime <- liftIO $ readTVarIO $ bsLastTime simulator
  when (uptime > fromtime)
    $ _simulateUntil simulator fromtime uptime

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

whenDef :: (Monad m) => Bool -> m a -> m a -> m a
whenDef False deflt _ = deflt
whenDef True _ action = action

tickSink :: (MonadIO m, PersistStore m, PersistMonadBackend m ~ SqlBackend)
            => BoardSimulator -> Sink (Entity Tick) m ()
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
          tt = tickTime tick
      postDone <- liftIO $ atomically $ case loDirection limitOrder of
        Buy -> do
          whenDef (tPrice <= lPrice) (return $ return ()) $ do
            money <- readTVar $ bsMoney sim
            let needMoney = (realToFrac lVol) * tPrice
            whenDef (money >= needMoney) (return $ return ())$ do
              writeTVar (bsMoney sim) $ money - needMoney
              modifyTVar' (bsTickers sim) (+ lVol)
              modifyTVar' (bsOrders sim) $ M.delete oid
              writeTVar (bsLastTime sim) tt
              return $ do
                insert_ $ Transaction { transactionBoard = boardName sim
                                      , transactionTicker = boardTickerName sim
                                      , transactionSessionId = bsSession sim
                                      , transactionDirection = Buy
                                      , transactionTime = tt
                                      , transactionPrice = needMoney
                                      , transactionVolume = lVol}

        Sell -> do
          whenDef (tPrice >= lPrice) (return $ return ()) $ do
            tickers <- readTVar $ bsTickers sim
            whenDef (tickers >= lVol) (return $ return ())$ do
              writeTVar (bsTickers sim) $ tickers - lVol
              let gotMoney = ((realToFrac lVol) * tPrice)
              modifyTVar' (bsMoney sim) (+ gotMoney)
              modifyTVar' (bsOrders sim) $ M.delete oid
              writeTVar (bsLastTime sim) $ tickTime tick
              return $ do
                insert_ $ Transaction { transactionBoard = boardName sim
                                      , transactionTicker = boardTickerName sim
                                      , transactionSessionId = bsSession sim
                                      , transactionDirection = Sell
                                      , transactionTime = tt
                                      , transactionPrice = gotMoney
                                      , transactionVolume = lVol}

      lift postDone
