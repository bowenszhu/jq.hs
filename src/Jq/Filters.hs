module Jq.Filters where

data Filter = Identity | ObjectIndex String

instance Show Filter where
  show (Identity     ) = "."
  show (ObjectIndex s) = "." ++ s

data Config = ConfigC
  { filters :: Filter
  }
