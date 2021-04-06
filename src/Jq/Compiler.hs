module Jq.Compiler where

import qualified Data.Map                      as Map
import           Jq.Filters
import           Jq.Json

-- import           Debug.Trace                    ( trace )

type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity inp = return [inp]
compile (ObjectIndex s _) (JObject m) =
  case Map.lookup (JString s) (Map.fromList m) of
    Nothing -> return [JNull]
    Just j  -> return [j]
compile (ObjectIndex _ _) JNull = return [JNull]
compile (ObjectIndex s b) _     = if b
  then Left ("error: Cannot index with string \"" ++ s ++ "\"")
  else return []
compile (ArrayIndex i _) (JArray a) = return [a !! i]
compile (ArrayIndex _ _) JNull      = return [JNull]
compile (ArrayIndex _ b) _ =
  if b then Left "error: Cannot index non-array" else return []
compile (ArraySlice s t _) (JArray a) =
  return [JArray [ a !! i | i <- [s .. min (t - 1) (length a - 1)] ]]
compile (ArraySlice s t _) (JString a) =
  return [JString [ a !! i | i <- [s + 1 .. min t (length a)] ]]
compile ArraySlice{} JNull = return [JNull]
compile (ArraySlice _ _ b) _ =
  if b then Left "error: Cannot slice non-array" else return []
compile (AllIterator _) (JObject m) = return [ v | (_, v) <- m ]
compile (AllIterator _) (JArray  a) = return a
compile (AllIterator _) JNull       = return [JNull]
compile (AllIterator b) _ =
  if b then Left "error: Cannot slice non-array/object" else return []
compile (ArrayIterator xs _) (JArray a) = return [ a !! i | i <- xs ]
compile (ArrayIterator _ b) _ =
  if b then Left "error: Cannot iterator non-array" else return []
compile (Comma []      ) inp = return [inp]
compile (Comma [f     ]) inp = compile f inp
compile (Comma (f : fs)) inp = case compile f inp of
  Left  s   -> Left s
  Right [r] -> case compile (Comma fs) inp of
    Left  t  -> Left t
    Right rs -> return (r : rs)
  _ -> Left "error: Cannot comma"
compile (Pipe []      ) inp = return [inp]
compile (Pipe [f     ]) inp = compile f inp
compile (Pipe (f : fs)) inp = case compile f inp of
  Left  s   -> Left s
  Right []  -> return []
  Right [r] -> compile (Pipe fs) r
  Right rs  -> helpCompilePipe fs rs
compile (FArray f) inp = case compile f inp of
  Left  s  -> Left s
  Right rs -> return [JArray rs]

helpCompilePipe :: [Filter] -> [JSON] -> Either String [JSON]
helpCompilePipe _  []       = return []
helpCompilePipe fs [x     ] = compile (Pipe fs) x
helpCompilePipe fs (x : xs) = case compile (Pipe fs) x of
  Left  m  -> Left m
  Right rs -> case helpCompilePipe fs xs of
    Left  n  -> Left n
    Right qs -> return (rs ++ qs)

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
