{-# LANGUAGE
  QuasiQuotes
, TypeFamilies
, GeneralizedNewtypeDeriving
, TemplateHaskell
, OverloadedStrings
, GADTs
, FlexibleContexts #-}


module Market.Board.Types where

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
import Data.Text (Text)
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Market.Models
import qualified Data.IntMap.Strict as M

newtype OrderId = OrderId {unOrderId :: Int}
                deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

newtype Money = Money {unMoney :: Rational}
                deriving (Enum, Eq, Fractional, Num, Ord, Read, Real, RealFrac, Show)

newtype Ticker = Ticker {unTicker :: Rational}
                deriving (Enum, Eq, Fractional, Num, Ord, Read, Real, RealFrac, Show)

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
