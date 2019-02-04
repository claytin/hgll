module Parser.General.Memo ( Memo
                           , memoize
                           , Parser.Data.Trie.empty ) where

import Parser.Data.Trie

-- Memo is essentialy a Trie, but it has some internal differences, this helps
-- to make that clear
type Memo v = Trie v

-- It takes a function from a string into some value of type b*, if memo m
-- already have a value indexed by args, just return v and the unmodified m.
-- Otherwise store the value resulting of (f args) in the memo, returnig the
-- value and the updated m.
-- * memoize should take any function of the form (a -> b), in this case,
-- however, that is not possible bacause (Trie v) is String indexed
memoize :: (String -> b) -> (Key -> Memo b -> (b, Memo b))
memoize f = \args m -> case fetch args m of
    Just v -> (v, m)
    _      -> let v  = f args
                  m' = add args v m
              in  (v, m')
