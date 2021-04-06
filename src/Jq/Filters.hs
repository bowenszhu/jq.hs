module Jq.Filters where

data Filter = Identity | ObjectIndex String Bool | Pipe [Filter]

instance Show Filter where
  show Identity          = "."
  show (ObjectIndex s b) = "." ++ s ++ (if b then "" else "?")
  show (Pipe fs        ) = concatMap show fs

data Config = ConfigC
  { filters :: Filter
  }
