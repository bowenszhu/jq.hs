module Jq.Filters where

data Filter = Identity | ObjectIndex String | Pipe [Filter]

instance Show Filter where
  show (Identity      ) = "."
  show (ObjectIndex s ) = "." ++ s
  show (Pipe        fs) = concatMap show fs

data Config = ConfigC
  { filters :: Filter
  }
