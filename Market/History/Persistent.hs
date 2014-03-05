{-# LANGUAGE
  QuasiQuotes
, TypeFamilies
, GeneralizedNewtypeDeriving
, TemplateHaskell
, OverloadedStrings
, GADTs
, FlexibleContexts #-}

module Market.History.Persistent where

import Control.Applicative
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM
import Control.Monad.Trans.Maybe
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Market.History.Types
import Market.Models

data HistoryPersistent = HistoryPersistent
                         { hpBoard :: Text
                         , hpTicker :: Text
                         , hpPool :: ConnectionPool
                         , hpLastTime :: TVar UTCTime
                         }

instance History HistoryPersistent where
  histBoardName hist = hpBoard hist
  histTickerName hist = hpTicker hist

instance TickHistory HistoryPersistent where
  getTicksTime hist = do
    (flip runSqlPersistMPool) (hpPool hist) $ do
      let bn = hpBoard hist
          tn = hpTicker hist
      runMaybeT $ do
        (Entity _ f) <- MaybeT $ listToMaybe <$> selectList
                        [TickBoard ==. bn,
                         TickTicker ==. tn]
                        [Asc TickTime, LimitTo 1]
        (Entity _ l) <- MaybeT $ listToMaybe <$> selectList
                        [TickBoard ==. bn,
                         TickTicker ==. tn]
                        [Desc TickTime, LimitTo 1]
        lst <- liftIO $ readTVarIO $ hpLastTime hist
        let ft = tickTime f
            lt = tickTime l
        guard $ lst <= ft || lst <= lt
        return (min lst ft, min lst lt)

  getTicks hist (f, t) = do
    lst <- readTVarIO $ hpLastTime hist
    let from = min lst f
        to = min lst t
    (flip runSqlPersistMPool) (hpPool hist) $ do
      let bn = hpBoard hist
          tn = hpTicker hist
      map entityVal <$> selectList
        [TickBoard ==. bn,
         TickTicker ==. tn,
         TickTime >=. from,
         TickTime <=. to]
        [Asc TickTime]
