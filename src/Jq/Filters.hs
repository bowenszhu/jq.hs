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

-- instance Show Filter where
--   show Identity          = "Identity."
--   show (ObjectIndex s b) = "ObjectIndex." ++ s ++ (if b then "" else "?")
--   show (ArrayIndex  i b) = "ArrayIndex.[" ++ show i ++ "]" ++ (if b then "" else "?")
--   show (ArraySlice s t b) =
--     "ArraySlice.[" ++ show s ++ ":" ++ show t ++ "]" ++ (if b then "" else "?")
--   show (AllIterator b) = "AllIterator.[]" ++ (if b then "" else "?")
--   show (ArrayIterator xs b) =
--     "ArrayIterator.[" ++ intercalate "," (map show xs) ++ "]" ++ (if b then "" else "?")
--   show (Comma  fs) = intercalate " Comma, " (map show fs)
--   show (Pipe   fs) = intercalate " Pipe| " (map show fs)
--   show (FArray f ) = "FArray[" ++ show f ++ "]"

data Config = ConfigC
  { filters :: Filter
  }
