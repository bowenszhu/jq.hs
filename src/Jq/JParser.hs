module Jq.JParser where

import           Jq.Json
import           Parsing.Parsing

parseJNull :: Parser JSON
parseJNull = do
  _ <- string "null"
  return JNull

parseInt :: Parser JSON
parseInt = do
  i <- int
  return (JInt i)

parseFloat :: Parser JSON
parseFloat = do
  f <- float
  return (JFloat f)

parseBool :: Parser JSON
parseBool =
  do
    _ <- string "true"
    return (JBool True)
  <|> 
  do
    _ <- string "false"
    return (JBool False)

parseString :: Parser JSON
parseString = do xs <- uni_str
                 return (JString xs)

parseJSON :: Parser JSON
parseJSON = token $ parseJNull <|> parseFloat <|> parseInt <|> parseBool <|> parseString
