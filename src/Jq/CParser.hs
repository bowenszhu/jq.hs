module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser (parseObject)
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
parseOp = parseObjectIndex <|> parseArrayIndex <|> parseArraySlice <|> parseValueIterator <|> parseIdentity <|> parseParenthesis <|> parseFArray

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
parsePipeElem = do _ <- symbol "|"
                   x <- parseOp
                   y <- parsePipeElem
                   return ((\(Pipe xs) -> Pipe (x:xs)) y)
                <|>
                do _ <- symbol "|"
                   x <- parseOp
                   return (Pipe [x])
                <|>
                do x <- parseOp
                   y <- parsePipeElem
                   return ((\(Pipe xs) -> Pipe (x:xs)) y)
                <|>
                do x <- parseOp
                   return (Pipe [x])

parsePipe :: Parser Filter
parsePipe = do x <- parseOp
               y <- parsePipeElem
               return ((\(Pipe xs) -> Pipe (x:xs)) y)

parsePipeSpec :: Parser Filter
parsePipeSpec = do x <- parseOp
                   (do y <- parsePipeSpec
                       return ((\(Pipe xs) -> Pipe (x:xs)) y)
                    <|>
                    return (Pipe [x]))

parseParenthesis :: Parser Filter
parseParenthesis = do _ <- symbol "("
                      x <- parseFilter
                      _ <- symbol ")"
                      return x

parseFArray :: Parser Filter
parseFArray = do _ <- symbol "["
                 xs <- parseFilter
                 _ <- symbol "]"
                 return (FArray xs)

parseFBool :: Parser Filter
parseFBool = do _ <- string "true"
                return (FBool True)
             <|>
             do _ <- string "false"
                return (FBool False)

parseFNull :: Parser Filter
parseFNull = do _ <- string "null"
                return FNull

parseFEObj :: Parser Filter
parseFEObj = do _ <- symbol "{"
                _ <- symbol "}"
                return FEObj

parseFEArray :: Parser Filter
parseFEArray = do _ <- symbol "["
                  _ <- symbol "]"
                  return FEArray

parseFObj :: Parser Filter
parseFObj = do j <- parseObject
               return (FObj j)
               
parseFilter :: Parser Filter
parseFilter = parseComma <|> parsePipe <|> parsePipeSpec <|> parseFBool <|> parseFEArray <|> parseFEObj <|> parseFObj <|> parseFNull <|> parseObjectIndex <|> parseArrayIndex <|> parseArraySlice <|> parseValueIterator <|> parseIdentity <|> parseParenthesis <|> parseFArray
               
parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
