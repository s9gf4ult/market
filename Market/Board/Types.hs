{-# LANGUAGE
  QuasiQuotes
, TypeFamilies
, GeneralizedNewtypeDeriving
, TemplateHaskell
, OverloadedStrings
, GADTs
, FlexibleContexts #-}


module Market.Board.Types where

import Data.Text (Text)
import Data.Time
import Market.Models.Fields
import Market.Models

newtype OrderId = OrderId {unOrderId :: Int}
                deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data LimitOrder = LimitOrder
                  { loTime :: UTCTime
                  , loDirection :: Direction
                  , loPrice :: Money
                  , loVolume :: Ticker
                  } deriving (Show, Eq, Ord)

data Order = LO LimitOrder
             deriving (Show, Eq, Ord)

orderTime :: Order -> UTCTime
orderTime (LO lo) = loTime lo

-- with Board instance you can work with some financial instrument on some
-- board.
class Board board where
  boardName :: board -> Text
  boardTickerName :: board -> Text
  boardMoneyName :: board -> Text
  listOrders :: board -> IO [(OrderId, Order)]
  cancelOrder :: board -> OrderId -> IO Bool
  moneyAmount :: board -> IO Money
  tickersAmount :: board -> IO Ticker

-- if board can set limit orders
class (Board board) => CanLimitOrder board where
  registerLimitOrder :: board -> LimitOrder -> IO OrderId

class (Board board) => HasHistory board where
  lastTransactions :: board -> Int -> IO [Transaction]
