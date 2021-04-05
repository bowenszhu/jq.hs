module Jq.JParser where

import           Jq.Json
import           Parsing.Parsing
import qualified Data.Map                      as Map

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

parseArrayElem :: Parser JSON
parseArrayElem = do _ <- symbol "]"
                    return (JArray [])
                 <|>
                 do x <- parseJSON
                    (do _ <- symbol "]"
                        return (JArray [x])
                     <|>
                     do _ <- symbol ","
                        a <- parseArrayElem
                        return ((\(JArray xs) -> JArray (x:xs)) a))

parseArray :: Parser JSON
parseArray = do _ <- symbol "["
                parseArrayElem

parseObjectElem :: Parser JSON
parseObjectElem = do _ <- symbol "}"
                     return (JObject Map.empty)
                  <|> 
                  do k <- parseString
                     _ <- symbol ":"
                     v <- parseJSON
                     (do _ <- symbol "}"
                         return (JObject (Map.singleton k v))
                      <|>
                      do _ <- symbol ","
                         xs <- parseObjectElem
                         return ((\(JObject m) -> JObject (Map.insert k v m)) xs))

parseObject :: Parser JSON
parseObject = do _ <- symbol "{"
                 parseObjectElem

parseJSON :: Parser JSON
parseJSON = token $ parseJNull <|> parseObject <|> parseArray <|> parseFloat <|> parseInt <|> parseBool <|> parseString
