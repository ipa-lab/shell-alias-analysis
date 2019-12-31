{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{- cabal:
build-depends: base, attoparsec, text, direct-sqlite, neat-interpolation
-}

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.Char
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.SQLite3 as SQL
import NeatInterpolation
import Prelude hiding (take, takeWhile)
import System.Environment
import System.IO
import Text.Printf

-------------------------------------------------------------------------------

main :: IO ()
main = do
  (dbFile:_) <- getArgs
  db <- open (T.pack dbFile)

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
    whenRight (parseOnly aliases content) $ mapM_ $ \Alias{..} -> do
      aliasId <- insert db aliasInsertStmt
        [ SQLInteger fileId
        , SQLText name
        , SQLText value
        ]
      whenRight (parseOnly commands value) $ imapM_ $ \(i, Command{..}) -> do
        commandId <- insert db commandInsertStmt
          [ SQLInteger aliasId
          , SQLText name
          , SQLInteger i
          , SQLInteger (if sudo then 1 else 0)
          , maybe SQLNull SQLText operator
          , SQLText (T.intercalate " " arguments)
          ]
        flip imapM_ arguments $ \(j, argument) -> do
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

whenRight :: Either a b -> (b -> IO ()) -> IO ()
whenRight (Left _) _ = return ()
whenRight (Right x) f = f x

imapM_ :: Integral i => ((i,a) -> IO ()) -> [a] -> IO ()
imapM_ f xs = mapM_ f (zip [0..] xs)

-------------------------------------------------------------------------------

data Alias = Alias
  { name     :: Text
  , value    :: Text
  }
  deriving Show

data Command = Command
  { name      :: Text
  , arguments :: [Text]
  , sudo      :: Bool
  , operator  :: Maybe Text
  }
  deriving Show

aliases :: Parser [Alias]
aliases = go
 where
  go          = skipToAlias *> tryAlias
  skipToAlias = manyTill anyChar "alias "
  tryAlias    = liftA2 (:) alias (go <|> return []) <|> go

  alias = do
    skipOptional "-g "
    skipSpaceNotNewlines
    name <- takeWhile1 (\c -> c /= '=' && not (isSpace c))
    "="
    value <- quoted '"' <|> quoted '\'' <|> takeTill isEndOfLine
    return Alias{..}

  quoted :: Char -> Parser Text
  quoted c = char c *> takeWhile (/= c)

skipOptional :: Alternative f => f a -> f ()
skipOptional p = void p <|> pure ()

skipSpaceNotNewlines :: Parser ()
skipSpaceNotNewlines = skipWhile (\c -> isSpace c && not (isEndOfLine c))

groupsBy :: [Text] -> [Text] -> [[Text]]
groupsBy seps = go [] []
 where
  go zs ys [] = reverse (map reverse (ys:zs))
  go zs [] (x:xs) = go zs [x] xs
  go zs ys (x:xs)
    | x `elem` seps = go (ys:zs) [x] xs
    | otherwise = go zs (x:ys) xs


commands :: Parser [Command]
commands = do
  tokens <- tokenize
  return $ map mkCommand $ groupsBy operators tokens
 where
  mkCommand = \case
    (op:"sudo":name:args) | isOp op -> Command name args True  (Just op)
    (op       :name:args) | isOp op -> Command name args False (Just op)
    (op            :args) | isOp op -> Command ""   args False (Just op)
    (   "sudo":name:args)           -> Command name args True   Nothing
    (          name:args)           -> Command name args False  Nothing
    (               args)           -> Command ""   args False  Nothing

  isOp op = op `elem` operators


tokenize :: Parser [Text]
tokenize = skipSpace *> go [] mempty
 where
  go !xs !t0 = peekChar >>= \case
    Just c
      | isSpace c         -> skipSpace     >>         go (   t0:xs) mempty
      | isOp c, T.null t0 -> munchOp       >>= \t1 -> go (t1   :xs) mempty
      | isOp c            -> munchOp       >>= \t1 -> go (t1:t0:xs) mempty
      | isQuote c         -> munchQuote c  >>= \t1 -> go        xs  (t0 <> t1)
      | otherwise         -> munch         >>= \t1 -> go        xs  (t0 <> t1)
    Nothing
      | T.null t0         -> return $ reverse     xs
      | otherwise         -> return $ reverse (t0:xs)

  munchOp      = choice (map string operators) <* skipSpace
  munchQuote c = take 1 <.> takeTill (== closing c) <.> option "" (take 1)
  munch        = takeTill (\c -> isQuote c || isSpace c || isOp c)

  isOp    c = c `elem` ['|',';','&']
  isQuote c = c `elem` ['"', '\'', '`', '(']

  closing '(' = ')'
  closing c   = c

operators = ["||", "&&", "|&", "|", ";", "&"]

(<.>) :: (Applicative f, Semigroup a) => f a -> f a -> f a
(<.>) = liftA2 (<>)

-------------------------------------------------------------------------------

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
