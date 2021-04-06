module Jq.Filters where

data Filter = Identity | ObjectIndex String Bool | ArrayIndex Int | Pipe [Filter]

instance Show Filter where
  show Identity          = "."
  show (ObjectIndex s b) = "." ++ s ++ (if b then "" else "?")
  show (ArrayIndex i   ) = ".[" ++ show i ++ "]"
  show (Pipe       fs  ) = concatMap show fs

data Config = ConfigC
  { filters :: Filter
  }
