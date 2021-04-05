module Jq.Json where

data JSON
  = JNull
  | JInt Int
  | JFloat Float
  | JBool Bool
  | JString String

instance Show JSON where
  show JNull           = "null"
  show (JInt    i    ) = show i
  show (JFloat  f    ) = show f
  show (JBool   True ) = "true"
  show (JBool   False) = "false"
  show (JString s    ) = s
