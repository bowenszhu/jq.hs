{-# OPTIONS_GHC -w #-}
-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

module Parsing.Parsing (module Parsing.Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char
-- import Debug.Trace (trace)

-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

isEscape :: Char -> Bool
isEscape c = c `elem` ['\'', '\"', '\\', '\n', '\r', '\t', '\b', '\f', '\v', '\xFF']

isSpec :: Char -> Bool
isSpec c = c `elem` ['-', '$', '@', '!', '`', '#', '%', '^', '&', '*', '(', ')', '_', '+', '=', '|']
-- isSpec c = c `elem` ['-', '$', '@', '!', '`', '#', '%', '^', '&', '*', '(', ')', '_', '+', '=', '|', ',', '.', ';', ':', '?', '<', '>', '[', ']']

cha :: Parser Char
cha = sat (\c -> isAlpha c || isEscape c || isSpace c || isSpec c || isDigit c)

hex :: Parser Char
hex = sat isHexDigit

hexChar :: Char -> Int
hexChar ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | ch == 'A' = 10
    | ch == 'B' = 11
    | ch == 'C' = 12
    | ch == 'D' = 13
    | ch == 'E' = 14
    | ch == 'F' = 15
    | ch == 'a' = 10
    | ch == 'b' = 11
    | ch == 'c' = 12
    | ch == 'd' = 13
    | ch == 'e' = 14
    | ch == 'f' = 15
    | otherwise     = 0

uni :: Parser Char
uni = do char '\\'
         char 'u'
         a <- hex
         b <- hex
         c <- hex
         d <- hex
         return (toEnum (((hexChar a * 16 + hexChar b) * 16 + hexChar c) * 16 + hexChar d) :: Char)

str :: Parser String
str = do many (uni <|> cha <|> char '_')

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many (alphanum <|> char '_')
           return (x:xs)

obj_ident :: Parser String
obj_ident = do x <- letter <|> char '_'
               xs <- many (alphanum <|> char '_')
               return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

u_float :: Parser Float
u_float = do xs <- some digit
             char '.'
             ys <- some digit
             (do string "E+"
                 zs <- some digit
                 return (read (xs ++ "." ++ ys ++ "E+" ++ zs) :: Float)
              <|>
              return (read (xs ++ "." ++ ys) :: Float))

float :: Parser Float
float = do char '-'
           uf <- u_float
           return (-uf)
         <|> u_float           

-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)
