module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
-- import Debug.Trace (trace)

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

parseObjectIndex :: Parser Filter
parseObjectIndex = do space
                      _ <- char '.'
                      (do xs <- obj_ident
                          (do _ <- char '?'
                              return (ObjectIndex ("\"" ++ xs ++ "\"") False)
                           <|>
                           return (ObjectIndex ("\"" ++ xs ++ "\"") True))
                       <|>
                       do _ <- char '\"'
                          xs <- obj_ident
                          _ <- char '\"'
                          (do _ <- char '?'
                              return (ObjectIndex ("\"" ++ xs ++ "\"") False)
                           <|>
                           return (ObjectIndex ("\"" ++ xs ++ "\"") True))
                       <|>
                       do _ <- string "[\""
                          xs <- obj_ident
                          _ <- string "\"]"
                          (do _ <- char '?'
                              return (ObjectIndex ("\"" ++ xs ++ "\"") False)
                           <|>
                           return (ObjectIndex ("\"" ++ xs ++ "\"") True)))

parseArrayIndex :: Parser Filter
parseArrayIndex = do _ <- symbol "."
                     _ <- symbol "["
                     i <- nat
                     _ <- symbol "]"
                     (do _ <- symbol "?"
                         return (ArrayIndex i False)
                      <|>
                      return (ArrayIndex i True))

parseArraySlice :: Parser Filter
parseArraySlice = do _ <- symbol "."
                     _ <- symbol "["
                     s <- nat
                     _ <- symbol ":"
                     t <- nat
                     _ <- symbol "]"
                     return (ArraySlice s t)

parseValueIteratorElem :: Parser Filter
parseValueIteratorElem = do d <- nat
                            (do _ <- symbol "]"
                                return (ArrayIterator [d])
                             <|>
                             do _ <- symbol ","
                                ds <- parseValueIteratorElem
                                return ((\(ArrayIterator xs)-> ArrayIterator (d:xs)) ds))

parseValueIterator :: Parser Filter
parseValueIterator = do _ <- symbol "."
                        _ <- symbol "["
                        (do _ <- symbol "]"
                            return ObjectIterator
                         <|>
                         parseValueIteratorElem)

parsePipe :: Parser Filter
parsePipe = do f <- parseObjectIndex
               (do _ <- symbol "|"
                   p <- parsePipe
                   return ((\(Pipe fs) -> Pipe (f:fs)) p)
                <|>
                do p <- parsePipe
                   return ((\(Pipe fs) -> Pipe (f:fs)) p)
                <|>
                return (Pipe [f]))

parseFilter :: Parser Filter
parseFilter = parsePipe <|> parseObjectIndex <|> parseArrayIndex <|> parseArraySlice <|> parseValueIterator <|> parseIdentity

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
