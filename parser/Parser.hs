{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Text (Text)
import Prelude hiding (take, takeWhile)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  test <- T.readFile "test.txt"
  case parseOnly aliases test of
    Left err -> print err
    Right xs -> mapM_ (T.putStrLn . pretty) xs

data Alias = Alias { name :: Text, value :: Text }
  deriving Show


pretty :: Alias -> Text
pretty Alias{..} = name <> "\t" <> value

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
