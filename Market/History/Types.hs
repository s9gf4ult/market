module Market.History.Types where

import Data.Text (Text)
import Data.Time
import Market.Models

class History hist where
  histBoardName :: hist -> Text
  histTickerName :: hist -> Text

class (History hist) => TickHistory hist where
  getTicksTime :: hist -> IO (Maybe (UTCTime, UTCTime))
  getTicks :: hist -> (UTCTime, UTCTime) -> IO [Tick]

class (History hist) => CandleHistory hist where
  getCandlesTime :: hist -> IO (Maybe (UTCTime, UTCTime))
  getCandles :: hist -> (UTCTime, UTCTime) -> IO [Candle]
