module Jq.Compiler where

import qualified Data.Map                      as Map
import           Jq.Filters
import           Jq.Json

-- import Debug.Trace (trace)

type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity inp = return [inp]
compile (ObjectIndex s _) (JObject m) =
  case Map.lookup (JString s) (Map.fromList m) of
    Nothing -> return [JNull]
    Just j  -> return [j]
compile (ObjectIndex s b) _ = if b
  then Left ("error: Cannot index with string \"" ++ s ++ "\"")
  else return []
compile (ArrayIndex i _) (JArray a) = return [a !! i]
compile (ArrayIndex _ b) _          = if b then Left "error: Cannot index non-array" else return []
compile (ArraySlice s t _) (JArray a) =
  return [JArray [ a !! i | i <- [s .. t - 1] ]]
compile (ArraySlice _ _ b) _ = if b then Left "error: Cannot slice non-array" else return []
compile (ObjectIterator _) (JObject m) = return [ v | (_, v) <- m ]
compile (ObjectIterator b) _ = if b then Left "error: Cannot slice non-object" else return []
compile (ArrayIterator xs) (JArray a) = return [ a !! i | i <- xs ]
compile (ArrayIterator _) _ = Left "error: Cannot iterator non-array"
compile (Pipe []) inp = return [inp]
compile (Pipe [f]) inp = compile f inp
compile (Pipe (f : fs)) inp = case compile f inp of
  Left  s   -> Left s
  Right [r] -> compile (Pipe fs) r
  _         -> Left "error: Cannot pipe"

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
