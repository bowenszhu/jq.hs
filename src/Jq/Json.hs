module Jq.Json where

data JSON
  = JNull
  | JInt Int
  | JFloat Float

instance Show JSON where
  show JNull      = "null"
  show (JInt   i) = show i
  show (JFloat f) = show f
