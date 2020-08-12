{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module SQL
  ( Database
  , open
  , close  
  , exec
  , Statement
  , prepare
  , finalize
  , insert
  , query1
  , queryForM_
  , SQLData(..)
  ) where

import Database.SQLite3 as SQL
import Data.Int
import Data.Text (Text)

insert :: Database -> Statement -> [SQLData] -> IO Int64
insert db stmt cols = do
  reset stmt
  bind stmt cols
  _ <- stepUntilDone stmt
  lastInsertRowId db

query1 :: Database -> Text -> IO [SQLData]
query1 db q = do
  stmt <- prepare db q
  xs <- head <$> stepUntilDone stmt
  finalize stmt
  return xs

queryForM_ :: Database -> Text -> (Int64 -> [SQLData] -> IO ()) -> IO ()
queryForM_ db q f = do
  stmt <- prepare db q
  stepWithCallbackUntilDone f stmt
  finalize stmt

stepWithCallbackUntilDone :: (Int64 -> [SQLData] -> IO ()) -> Statement -> IO ()
stepWithCallbackUntilDone f stmt = go 0
 where
  go !i = stepNoCB stmt >>= \case
    SQL.Done -> return ()
    SQL.Row  -> columns stmt >>= f i >> go (i+1)

stepUntilDone :: Statement -> IO [[SQLData]]
stepUntilDone stmt = go []
 where
  go !ys = stepNoCB stmt >>= \case
    SQL.Done -> return $! reverse ys
    SQL.Row  -> columns stmt >>= \y -> go (y:ys)
