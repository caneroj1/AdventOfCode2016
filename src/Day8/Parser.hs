module Day8.Parser
(
  parseAction
, Parsed
) where

import Data.Text (Text)
import Day8.Types
import Text.Parsec

type Parser a = Parsec Text () a
type Parsed = Either ParseError Cmd

parseAction :: Text -> Either ParseError Cmd
parseAction = parse tryParseAction ""

tryParseAction :: Parser Cmd
tryParseAction =
  try (string "rect" >> parseRect)   <|>
  (string "rotate" >> parseRotation)

parseRect :: Parser Cmd
parseRect = do
  space
  w <- read <$> many1 digit
  char 'x'
  h <- read <$> many1 digit
  return $ Rect w h

parseRotation :: Parser Cmd
parseRotation = do
  space
  f <- a
  space
  letter
  char '='
  i <- read <$> many1 digit
  space
  string "by"
  space
  n <- read <$> many1 digit
  return $ f i n
  where
    a = (string "row" >> return Row) <|> (string "column" >> return Col)
