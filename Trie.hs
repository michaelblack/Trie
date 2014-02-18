module Trie (Trie, emptyTrie, lookup, insert, toList, completions, connections) where 

import qualified Data.Map as Map
import Data.Maybe
import Control.Applicative
import Prelude hiding (lookup)
import Data.List hiding (insert, lookup)

data Trie k v = Trie {value :: !(Maybe v),
                      children :: !(Map.Map k (Trie k v))}
              deriving Show

-- | Contructs an empty Trie
emptyTrie :: Trie k v
emptyTrie = Trie Nothing Map.empty

-- | Looks up a trie node by its key string
lookupSub :: Ord k => [k] -> Trie k v -> Maybe (Trie k v)
lookupSub [] trie = Just trie
lookupSub (k:ks) trie = let found = Map.lookup k (children trie)
                        in if isNothing found
                           then Nothing
                           else lookupSub ks (fromJust found)

-- | Looks up a value by its key string
lookup :: Ord k => [k] -> Trie k v -> Maybe v
lookup keys trie = lookupSub keys trie >>= value

-- | Inserts a key into a trie at the location of a key string
insert :: Ord k => [k] -> v -> Trie k v -> Trie k v
insert [] v trie = Trie (Just v) (children trie)
insert (k:ks) v trie = let found = Map.lookup k (children trie)
                       in if isNothing found
                          then Trie (value trie) $ Map.insert k (insert ks v emptyTrie) (children trie)
                          else Trie (value trie) $ Map.insert k (insert ks v (fromJust found)) (children trie)

-- | Gives a list of keys connected to any Trie Node
connections :: Trie k v -> [k]
connections = map fst . Map.toList . children

toList' :: Ord k => [k] -> Trie k v -> [([k], v)]
toList' acc trie = if isNothing (value trie)
                   then (concatMap (\ (k,t) -> toList' (acc++[k]) t) $ Map.toList (children trie))
                   else (acc, fromJust $ value trie) : (concatMap (\ (k,t) -> toList' (acc++[k]) t) $ Map.toList (children trie)) 


-- | Returns an in order list of key string, value pairs in order that are contained in the Trie
toList :: Ord k => Trie k v -> [([k], v)]
toList = toList' []

-- | Returns a list of string keys starting from the Trie located at the key string
completions :: Ord k => [k] -> Trie k v -> [[k]]
completions s trie = let found = lookupSub s trie
                      in if isNothing found
                         then []
                         else map fst . toList $ (fromJust found)
