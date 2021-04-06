module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
-- import Debug.Trace (trace)

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

parseObjectIndex :: Parser Filter
parseObjectIndex = do _ <- char '.'
                      xs <- obj_ident
                      return (ObjectIndex ("\"" ++ xs ++ "\""))

parseFilter :: Parser Filter
parseFilter = parseObjectIndex <|> parseIdentity

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
