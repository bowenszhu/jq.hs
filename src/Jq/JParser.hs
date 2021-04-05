module Jq.JParser where

import           Jq.Json
import           Parsing.Parsing

parseJNull :: Parser JSON
parseJNull = do
  _ <- string "null"
  return JNull

parseInt :: Parser JSON
parseInt = do
  integral <- int
  return (JInt integral)

parseJSON :: Parser JSON
parseJSON = token $ parseJNull <|> parseInt
