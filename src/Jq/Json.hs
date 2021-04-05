module Jq.Json where

data JSON
  = JNull
  | JInt Int

instance Show JSON where
  show JNull      = "null"
  show (JInt   i) = show i
