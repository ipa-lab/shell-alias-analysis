{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{- cabal:
build-depends: base, attoparsec, text
-}

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Prelude hiding (take, takeWhile)
import Text.Printf

main :: IO ()
main = do
  test <- T.readFile "test.txt"
  case parseOnly aliases test of
    Left err -> print err
    Right xs -> forM_ xs $ \Alias{..} -> do
      printf "alias %s=%s\n" name value
      case parseOnly commands value of
        Left err -> print err
        Right ys -> forM_ ys $ \Command{..} -> do
          T.putStr "  "
          case preOp of
            Nothing -> pure ()
            Just op -> T.putStr $ op <> " "
          when sudo (putStr "sudo ")
          T.putStr $ name <> " "
          T.putStr $ T.intercalate " " arguments
          T.putStr "\n"
      T.putStr "\n"

data Alias = Alias
  { name     :: Text
  , value    :: Text
  }
  deriving Show

data Command = Command
  { name      :: Text
  , arguments :: [Text]
  , sudo      :: Bool
  , preOp     :: Maybe Text
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
    name <- takeWhile1 (inClass "a-zA-Z0-9_!%,@-:")
    "="
    value <- quoted '"' <|> quoted '\'' <|> takeTill isEndOfLine
    return Alias{..}

  quoted :: Char -> Parser Text
  quoted c = char c *> takeWhile (/= c)

skipOptional :: Alternative f => f a -> f ()
skipOptional p = void p <|> pure ()


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
