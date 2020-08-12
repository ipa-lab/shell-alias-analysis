{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Monad.Extra
import Data.Text qualified as Text
import NeatInterpolation
import System.Environment
import System.IO
import Text.Printf

import Parser
import SQL

main :: IO ()
main = do
  (dbFile:_) <- getArgs
  db <- open (Text.pack dbFile)

  exec db [text|
    CREATE TABLE IF NOT EXISTS alias
    ( alias_id INTEGER PRIMARY KEY
    , file_id INTEGER NOT NULL
    , name TEXT NOT NULL
    , value TEXT NOT NULL
    , FOREIGN KEY (file_id) REFERENCES file(file_id)
    );
    CREATE TABLE IF NOT EXISTS command
    ( command_id INTEGER PRIMARY KEY
    , alias_id INTEGER NOT NULL
    , name TEXT NOT NULL
    , position INTEGER NOT NULL
    , sudo INTEGER NOT NULL
    , operator TEXT
    , arguments TEXT NOT NULL
    , FOREIGN KEY (alias_id) REFERENCES alias(alias_id)
    );
    CREATE TABLE IF NOT EXISTS argument
    ( argument_id INTEGER PRIMARY KEY
    , command_id INTEGER NOT NULL
    , name TEXT NOT NULL
    , position INTEGER NOT NULL
    , FOREIGN KEY (command_id) REFERENCES command(command_id)
    );
  |]

  aliasInsertStmt <- prepare db
    "INSERT INTO alias (file_id, name, value) VALUES (?,?,?)"
  commandInsertStmt <- prepare db [text|
    INSERT INTO command (alias_id,name,position,sudo,operator,arguments)
    VALUES (?,?,?,?,?,?)
  |]
  argumentInsertStmt <- prepare db
    "INSERT INTO argument (command_id,name,position) VALUES (?,?,?)"

  putStr "Counting..."

  [SQLInteger total] <- query1 db "SELECT COUNT(*) FROM file"
  [SQLInteger completed] <- query1 db [text|
    SELECT COUNT(*) FROM file
    LEFT JOIN alias USING (file_id)
    WHERE alias_id IS NOT NULL
  |]

  hSetBuffering stdout NoBuffering

  let updateProgress n = do
        let percent = (fromIntegral n / fromIntegral total) * 100 :: Double
        printf "\r%7d / %7d (%6.2f%%)" n total percent

  updateProgress completed

  let fileSelectQ = [text|
        SELECT file_id, content FROM file
        LEFT JOIN alias USING (file_id)
        WHERE alias_id IS NULL
      |]
  queryForM_ db fileSelectQ $ \row [SQLInteger fileId, SQLText content] -> do
    whenJust (parseAliases content) $ mapM_ $ \Alias{..} -> do
      aliasId <- insert db aliasInsertStmt
        [ SQLInteger fileId
        , SQLText name
        , SQLText value
        ]
      forM_ (zip [0..] commands) $ \(i, Command{..}) -> do
        commandId <- insert db commandInsertStmt
          [ SQLInteger aliasId
          , SQLText name
          , SQLInteger i
          , SQLInteger (if sudo then 1 else 0)
          , maybe SQLNull SQLText operator
          , SQLText (Text.unwords arguments)
          ]
        forM_ (zip [0..] arguments) $ \(j, argument) -> do
          void $ insert db argumentInsertStmt
            [ SQLInteger commandId
            , SQLText argument
            , SQLInteger j
            ]
    updateProgress (completed + row + 1)

  finalize aliasInsertStmt
  finalize commandInsertStmt
  finalize argumentInsertStmt

  close db
