module Jq.Json where

import           Data.List
-- import Debug.Trace (trace)

data JSON
  = JNull
  | JInt Int
  | JFloat Float
  | JBool Bool
  | JString String
  | JArray [JSON]
  | JObject [(JSON, JSON)]

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
  show (JArray  []   ) = "[]"
  show (JArray xs) =
    "[\n  "
      ++ intercalate "\n  " (lines (intercalate ",\n" (map show xs)))
      ++ "\n]"
  show (JObject []) = "{}"
  show (JObject m) =
    "{\n  "
      ++ intercalate
           "\n  "
           (lines
             (intercalate ",\n" (map (\(k, v) -> show k ++ ": " ++ show v) m))
           )
      ++ "\n}"
