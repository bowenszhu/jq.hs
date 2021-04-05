module Jq.JParser where

import Jq.Json
import Parsing.Parsing

parseJNull :: Parser JSON
parseJNull = do
  _ <- string "null"
  return JNull

parseUnsigned :: Parser JSON
parseUnsigned = do
  xs <- some digit
  (do
      _ <- char '.'
      (do
          ys <- some digit
          if (read ys :: Int) == 0
          then return (JInt (read xs))
          else return (JFloat (read (xs ++ "." ++ ys) :: Float))
       <|> 
       return (JInt (read xs)))
   <|> 
   return (JInt (read xs)))                 

parseJSON :: Parser JSON
parseJSON = token $ parseJNull <|> parseUnsigned
