module Jq.JParser where

import Jq.Json
import Parsing.Parsing

parseJNull :: Parser JSON
parseJNull = do
  _ <- string "null"
  return JNull

parseInt :: Parser JSON
parseInt = do i <- int
              return (JInt i)

parseFloat :: Parser JSON
parseFloat = do f <- float
                return (JFloat f)

parseJSON :: Parser JSON
parseJSON = token $ parseJNull <|> parseFloat <|> parseInt
