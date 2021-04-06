module Jq.Filters where

import           Data.List

data Filter = Identity | ObjectIndex String Bool | ArrayIndex Int Bool | ArraySlice Int Int Bool | ObjectIterator Bool | ArrayIterator [Int] | Pipe [Filter]

instance Show Filter where
  show Identity          = "."
  show (ObjectIndex s b) = "." ++ s ++ (if b then "" else "?")
  show (ArrayIndex  i b) = ".[" ++ show i ++ "]" ++ (if b then "" else "?")
  show (ArraySlice s t b) =
    ".[" ++ show s ++ ":" ++ show t ++ "]" ++ (if b then "" else "?")
  show (ObjectIterator b ) = ".[]" ++ (if b then "" else "?")
  show (ArrayIterator  xs) = ".[" ++ intercalate "," (map show xs) ++ "]"
  show (Pipe           fs) = concatMap show fs

data Config = ConfigC
  { filters :: Filter
  }
