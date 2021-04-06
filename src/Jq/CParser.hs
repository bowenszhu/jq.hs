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
                     (do _ <- symbol "?"
                         return (ArraySlice s t False)
                      <|>
                      return (ArraySlice s t True))

parseValueIteratorElem :: Parser Filter
parseValueIteratorElem = do d <- nat
                            (do _ <- symbol "]"
                                (do _ <- symbol "?"
                                    return (ArrayIterator [d] False)
                                 <|>
                                 return (ArrayIterator [d] True))
                             <|>
                             do _ <- symbol ","
                                ds <- parseValueIteratorElem
                                return ((\(ArrayIterator xs b)-> ArrayIterator (d:xs) b) ds))

parseValueIterator :: Parser Filter
parseValueIterator = do _ <- symbol "."
                        _ <- symbol "["
                        (do _ <- symbol "]"
                            (do _ <- symbol "?"
                                return (AllIterator False)
                             <|>
                             return (AllIterator True))
                         <|>
                         parseValueIteratorElem)

parseOp :: Parser Filter
parseOp = parseObjectIndex <|> parseArrayIndex <|> parseArraySlice <|> parseValueIterator <|> parseIdentity

parseCommaElem :: Parser Filter
parseCommaElem = do x <- parseOp
                    (do _ <- symbol ","
                        y <- parseCommaElem
                        return ((\(Comma xs) -> Comma (x:xs)) y)
                     <|>
                     return (Comma [x]))

parseComma :: Parser Filter
parseComma = do x <- parseOp
                _ <- symbol ","
                y <- parseCommaElem
                return ((\(Comma xs) -> Comma (x:xs)) y)

parsePipeElem :: Parser Filter
parsePipeElem = do x <- parseOp
                   (do _ <- symbol "|"
                       y <- parsePipeElem
                       return ((\(Pipe xs) -> Pipe (x:xs)) y)
                    <|>
                    return (Pipe [x]))

parsePipe :: Parser Filter
parsePipe = do x <- parseOp
               _ <- symbol "|"
               y <- parsePipeElem
               return ((\(Pipe xs) -> Pipe (x:xs)) y)

parseFilter :: Parser Filter
parseFilter = parseComma <|> parsePipe <|> parseObjectIndex <|> parseArrayIndex <|> parseArraySlice <|> parseValueIterator <|> parseIdentity

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
