module Jq.Json where

import           Data.List
import qualified Data.Map                      as Map

data JSON
  = JNull
  | JInt Int
  | JFloat Float
  | JBool Bool
  | JString String
  | JArray [JSON]
  | JObject (Map.Map JSON JSON)

instance Eq JSON where
  (JString a) == (JString b) = a == b
  _           == _           = True

instance Ord JSON where
  (JString a) < (JString b) = a < b
  JString _   < _           = True
  _           < _           = False
  x <= y = x < y || x == y

instance Show JSON where
  show JNull           = "null"
  show (JInt    i    ) = show i
  show (JFloat  f    ) = show f
  show (JBool   True ) = "true"
  show (JBool   False) = "false"
  show (JString s    ) = s
  show (JArray  xs   ) = "[" ++ intercalate "," (map show xs) ++ "]"
  show (JObject m) =
    "{"
      ++ intercalate
           ","
           (map (\(k, v) -> show k ++ ":" ++ show v) (reverse (Map.toList m)))
      ++ "}"
