{-# LANGUAGE
  QuasiQuotes
, TypeFamilies
, GeneralizedNewtypeDeriving
, TemplateHaskell
, OverloadedStrings
, GADTs
, FlexibleContexts #-}


module Main where

import Test.Hspec
import Market.Board
import Data.IORef
import Database.Persist.Sqlite

setioref :: IORef (Maybe ConnectionPool) -> IO ()
setioref ioref = do
  p <- createSqlitePool ":memory:" 1
  writeIORef ioref $ Just p


main :: IO ()
main = do
  pool <- newIORef Nothing
  hspec $ before (setioref pool) $ do
    describe "trying to buy" $ do
      return ()
