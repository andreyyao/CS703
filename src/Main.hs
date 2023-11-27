module Main where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Ast
import Typing(Context)

-- removeDups takes a context and remove the variables with duplicate types
removeDups :: Context -> [(String, Tipe)]
removeDups m =
  helper Set.empty (Map.toList m) where
  helper seen lst =
    case lst of
      [] -> []
      (v, t) : xs ->
        if Set.member t seen then helper seen xs else (v, t) : (helper (Set.insert t seen) xs)

myMap :: Map.Map String Tipe
myMap = Map.fromList [("x", TInt), ("y", TBool), ("z", TInt), ("h", TInt)]

uniqueKeyValuePairs :: [(String, Tipe)]
uniqueKeyValuePairs = removeDups myMap

-- Print the unique key-value pairs
main :: IO ()
main = do
  putStrLn "Unique key-value pairs:"
  mapM_ print uniqueKeyValuePairs