module Parser.Data.Trie ( Trie(..)
                        , Key
                        , empty
                        , fetch
                        , add ) where

import qualified Data.Map as Map

-- Some aliases, cause readability
type Key = String
type Map = Map.Map

-- A Trie is a string indexed , where each sub-tree/node can have
-- (Maybe) a value v, in this case the path leading to such node is a key that
-- indexes v, and a list (Map) of children representin character (Link) labeled
-- paths to other indexes that have the current path as prefix
-- data Trie v = Node (Maybe v) (Map Link (Trie v)) deriving Show
data Trie v = Trie { value :: Maybe v
                   , links :: Map Char (Trie v) }

-- An empty trie
empty :: Trie v
empty = Trie { value = Nothing , links = Map.empty }

-- Given a Key (k:ks) check if the current trie has a link for k, if not, there
-- is no value in the trie indexed bu (k:ks), otherwise proceed to the sub-tree
-- for which the path is labeled by k carrying the raminder of the key, ks,
-- when key = [ ] we walked through the corresponding path in the trie, then
-- return the value indexed by the key
fetch          :: Key -> Trie v -> Maybe v
fetch [ ]    t = value t
fetch (k:ks) t = case Map.lookup k (links t) of
    Just t' -> fetch ks t'
    _       -> Nothing

-- It is in some ways similar to fetch, we walk down the path for the given
-- Key (k:ks), if we walked through the hole path, return a trie that now
-- stores v, otherwise see if the current trie has a link for k, if no link is
-- found, add a new link for k and proced to the new added node, if k already
-- is a link proceed to the correspondent sub-tree
add            :: Key -> v -> Trie v -> Trie v
add [ ]    v t = Trie { value = Just v, links = links t }
add (k:ks) v t =
    let trie  = case Map.lookup k (links t) of
                     Nothing -> empty
                     Just t' -> t'
        trie' = add ks v trie
    -- returns a new tree that
    in Trie { value = value t                        -- preservs the value of t
            , links = Map.insert k trie' (links t) } -- updates the links of t

-- Pretty printing
-- Don't worry about it, there is nothing pretty about pretty printing

-- Utility show functions
indent   :: Int -> String
indent n = (concat . replicate n) "  "

align   :: Int -> String
align n = newLine ++ indent n
          where newLine = "\n"

links' :: Trie v -> [(Char, Trie v)]
links' = Map.toList . links

-- The structure of the definitions of indentShow and show hopefully depicts
-- how the tree is displayed
indentShow               :: (Show v) => Int -> [(Char, Trie v)] -> String
indentShow n [ ]         = ""
indentShow n ((k, t):ts) =
    align n ++ "[.(" ++ [k] ++ ", " ++ value' t ++ ")"
            ++ indentShow (n + 1) (links' t) ++ " ]"
            ++ indentShow n ts
    where value' trie = case value trie of
              Nothing -> ""
              Just v  -> show v

instance (Show v) => Show (Trie v) where
    show t = "[.(, )"
          ++ indentShow 1 (links' t) ++ " ]"
