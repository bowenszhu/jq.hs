module Jq.Filters where

import           Data.List

data Filter = Identity | ObjectIndex String Bool | ArrayIndex Int Bool | ArraySlice Int Int Bool | AllIterator Bool | ArrayIterator [Int] Bool | Comma [Filter] | Pipe [Filter] | FArray Filter

instance Show Filter where
  show Identity          = "."
  show (ObjectIndex s b) = "." ++ s ++ (if b then "" else "?")
  show (ArrayIndex  i b) = ".[" ++ show i ++ "]" ++ (if b then "" else "?")
  show (ArraySlice s t b) =
    ".[" ++ show s ++ ":" ++ show t ++ "]" ++ (if b then "" else "?")
  show (AllIterator b) = ".[]" ++ (if b then "" else "?")
  show (ArrayIterator xs b) =
    ".[" ++ intercalate "," (map show xs) ++ "]" ++ (if b then "" else "?")
  show (Comma  fs) = intercalate " , " (map show fs)
  show (Pipe   fs) = intercalate " | " (map show fs)
  show (FArray f ) = "[" ++ show f ++ "]"

data Config = ConfigC
  { filters :: Filter
  }
