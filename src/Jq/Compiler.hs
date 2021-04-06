module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import qualified Data.Map as Map
-- import Debug.Trace (trace)


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile (Identity) inp = return [inp]
compile (ObjectIndex s) (JObject m) = 
    case Map.lookup (JString s) (Map.fromList m) of
        Nothing -> return [JNull]
        Just j -> return [j]
compile (ObjectIndex s) _ = Left ("error: Cannot index with string \"" ++ s ++ "\"")

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
