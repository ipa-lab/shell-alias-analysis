{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Parser
  ( Alias(..)
  , Command(..)
  , parseAliases
  ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.Char
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Prelude hiding (take)

------------------------------------------------------------------------------

data Alias = Alias
  { name     :: Text
  , value    :: Text -- ^ normalized, not verbatim
  , commands :: [Command]
  }
  deriving Show

data Command = Command
  { name      :: Text
  , arguments :: [Text]
  , sudo      :: Bool
  , operator  :: Maybe Text
  }
  deriving Show

------------------------------------------------------------------------------

parseAliases :: Text -> Maybe [Alias]
parseAliases = either (const Nothing) Just . parseOnly aliases

aliases :: Parser [Alias]
aliases = tryAlias <|> nextLine
 where
   tryAlias = liftA2 (++) (skipSpace *> alias) (aliases <|> return [])
   nextLine = skipToEndOfLine *> endOfLine *> aliases

alias :: Parser [Alias]
alias = do
  keyword "alias"
  optional "-g "
  definitions <- many1' definition
  endOfLine <|> endOfInput <|> skip isComment
  return $ map mkAlias definitions

 where
   definition = do
    (name, q) <- takeName
    "="
    tokens <- takeValue q
    skipHorizontalSpace
    return (name, tokens)
  
mkAlias :: (Text, [Text]) -> Alias
mkAlias (name, tokens) = Alias 
  { name = name
  , value = Text.unwords tokens
  , commands = map mkCommand $ groupsBy operators tokens 
  }

mkCommand :: [Text] -> Command
mkCommand tokens = case tokens of
  (op:"sudo":name:args) | isOperator op -> Command name args True  (Just op)
  (op       :name:args) | isOperator op -> Command name args False (Just op)  
  (op            :args) | isOperator op -> Command ""   args False (Just op)
  (   "sudo":name:args)                 -> Command name args True   Nothing
  (          name:args)                 -> Command name args False  Nothing
  (               args)                 -> Command ""   args False  Nothing   

-- | @groupsBy seps xs@ breaks @xs@ into groups separated by @seps@.
groupsBy :: [Text] -> [Text] -> [[Text]]
groupsBy seps = go [] []
 where
  go zs ys [] = reverse (map reverse (ys:zs))
  go zs [] (x:xs) = go zs [x] xs
  go zs ys (x:xs)
    | x `elem` seps = go (ys:zs) [x] xs
    | otherwise = go zs (x:ys) xs

------------------------------------------------------------------------------

-- | Parses a (possibly quoted) alias name and returns the name
-- and maybe an open quote from the beginning (indicating that the whole
-- alias expression is in quotes, e.g. alias 'name=value').
takeName :: Parser (Text, Maybe Char)
takeName = do
  q1 <- maybeChar isQuote
  name <- takeTill (\c -> c == '=' || isSpace c || isQuote c)
  guard $ not $ Text.null name
  q2 <- maybeChar isQuote
  q3 <- case (q1, q2) of
    (Nothing, Nothing) -> return Nothing
    (Nothing, Just __) -> fail "unexpected quotation mark"
    (Just c1, Nothing) -> return (Just c1)
    (Just c1, Just c2) 
      | c2 `closes` c1 -> return Nothing
      | otherwise      -> fail "mismatched quotation mark"
  return (name, q3)

-- | Parses the value part of an alias definition, taking into account
-- possible outer quotation marks (e.g. alias a='b') which shouldn't be part
-- of the parsed value itself, as well as possibly nested outer quotation 
-- marks (e.g. alias 'a=(b)').
takeValue :: Maybe Char -> Parser [Text]
takeValue q0 = do
  q1 <- maybeChar isQuote
  case (q0, q1) of
    (Nothing, Nothing) -> tokenize Nothing
    (Nothing, Just c2) -> tokenize (Just c2)
    (Just c1, Nothing) -> tokenize (Just c1)
    (Just c1, Just c2)
      | c2 `closes` c1 -> return []
      | otherwise      -> tokenize (Just c2) <* char (closingQuote c1)

-- | Parses an alias value into individual tokens, taking care of quotes,
-- comments, whitespace, etc.
tokenize :: Maybe Char -> Parser [Text]
tokenize q0 = go [] mempty
 where
  go !xs !t0 = 
    let continue t1 = go xs (t0 <> t1)
        next        = go (cons' t0 xs) mempty
        done        = closeOuterQuote >> return (reverse $ cons' t0 xs)
    in peekChar >>= \case
      Nothing -> done
      Just c
        | isSpaceInOuterQuote   c -> skipSpace           >>  next
        | isSpace               c ->                         done
        | isCommentOutsideQuote c -> skipToEndOfLine     >>  done
        | isClosingOuterQuote   c ->                         done
        | isQuote               c -> takeQuote           >>= continue
        | isEscape              c -> goEscape xs t0
        | isOperatorPrefix      c -> goOperator xs t0
        | otherwise               -> takeTill isStopChar >>= continue

  goEscape xs t0 = 
    let continue c = go xs (t0 `Text.snoc` c)
        next       = go (cons' t0 xs) mempty
    in do
      c1 <- satisfy isEscape
      peekChar >>= \case
        Nothing -> continue c1
        Just c2
          | c2 == '\n'      -> take 1 *> next
          | c2 == ' '       -> take 1 *> continue ' '
          | isOuterQuote c2 -> take 1 *> continue c2
          | otherwise       ->           continue c1

  goOperator xs t0 = 
    let continue c1 = go xs (t0 `Text.snoc` c1)
        next     t1 = go (t1 : cons' t0 xs) mempty
    in do
      let c0 = fmap snd $ Text.unsnoc t0
      c1 <- satisfy isOperatorPrefix
      c2 <- peekChar
      case (c0,c1,c2) of
        (Just '>', '&', _       ) ->           continue c1
        (Just '<', '&', _       ) ->           continue c1
        (Just '>', '|', _       ) ->           continue c1
        (_       , '&', Just '>') ->           continue c1
        (_       , '|', Just '>') ->           continue c1    
        (_       , '|', Just '|') -> take 1 *> next "||"
        (_       , '|', Just '&') -> take 1 *> next "|&"
        (_       , '|', _       ) ->           next "|"
        (_       , '&', Just '&') -> take 1 *> next "&&"
        (_       , '&', _       ) ->           next "&"
        (_       , ';', _       ) ->           next ";"
        _ -> error "impossible"
    

  isSpaceInOuterQuote c = isSpace c && q0 /= Nothing
  isCommentOutsideQuote c = isComment c && q0 == Nothing
  isClosingOuterQuote c = maybe False (c `closes`) q0
  isOuterQuote c = maybe False (==c) q0 || isClosingOuterQuote c
  
  closeOuterQuote = case q0 of
    Nothing -> return ()
    Just c  -> void $ char (closingQuote c)
  
  isStopChar c = any ($ c) 
    [isSpace, isCommentOutsideQuote, isQuote, isEscape, isOperatorPrefix]

-- | Consumes a quoted string, including quotation marks.
-- Handles escaped quotes like @"\""@, but not nested quotes like @(())@.
takeQuote :: Parser Text
takeQuote = do
  c1 <- satisfy isOpeningQuote
  t <- go c1 mempty
  c2 <- satisfy (== closingQuote c1)
  return $ c1 `Text.cons` t `Text.snoc` c2
 where
   go q !t0 = do
    t <- takeTill (== closingQuote q)
    case fmap snd $ Text.unsnoc t of
      Just '\\' -> do
        c2 <- satisfy (== closingQuote q)
        go q (t0 <> t `Text.snoc` c2)
      _ -> return (t0 <> t)

isComment :: Char -> Bool
isComment c = c == '#'

isOpeningQuote, isClosingQuote, isQuote, isEscape, isOperatorPrefix :: Char -> Bool
isOpeningQuote c = c == '"' || c == '\'' || c == '`' || c == '('
isClosingQuote c = c == '"' || c == '\'' || c == '`' || c == ')'
isQuote c = c == '"' || c == '\'' || c == '`' || c == '(' || c == ')'
isEscape c = c == '\\'
isOperatorPrefix c = c == '&' || c == '|' || c == ';'

operators :: [Text]
operators = ["||", "&&", "|&", "|", ";", "&"]

isOperator :: Text -> Bool
isOperator = flip elem operators

-- | Returns an opening quotation mark's corresponding closing quotation mark.
closingQuote :: Char -> Char
closingQuote '(' = ')'
closingQuote c   = c

-- | @c2 `closes` c1@ returns `True` if @c2@ is the closing quotation mark for @c1@.
closes :: Char -> Char -> Bool
closes c2 c1 = c2 == closingQuote c1

-- | Skips over horizontal whitespace only.
skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile isHorizontalSpace

-- | Skips to the end of the line.
skipToEndOfLine :: Parser ()
skipToEndOfLine = skipWhile (not . isEndOfLine)

-- | @keyword t@ parses the string @t@, ignoring surrounding horizontal whitespace.
keyword :: Text -> Parser ()
keyword t = skipHorizontalSpace >> string t >> skipHorizontalSpace

-- | Consumes a character only if it matches a predicate.
maybeChar :: (Char -> Bool) -> Parser (Maybe Char)
maybeChar p = peekChar >>= \case
  Just c | p c -> take 1 >> return (Just c)
  _            ->           return Nothing

-- | Adds an element to the front of a list only if it is not @mempty@.
cons' :: (Monoid a, Eq a) => a -> [a] -> [a]
cons' x xs = if x == mempty then xs else (x:xs)
