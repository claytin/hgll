module Parser.Generator.Util ( gTerm
                             , gOpt
                             , gClojure, gStar
                             , gTimes ) where

import Parser.Data.ParseTree

gTerm            :: ParseTree -> String
gTerm (Token tk) = tk

-- The first equation of the following functions (Rule ...) unpack the actual
-- structure of the tree and it is necessary because of the recursive pattern
-- described by them
gOpt     :: (ParseTree -> String) -> ParseTree -> String
gOpt g t = case t of
    (Rule "Optional" t') -> gOpt g t'
    Eps                  -> ""
    _                    -> g t

gClojure     :: (ParseTree -> String) -> ParseTree -> String
gClojure g t = case t of
    (Rule "Clojure" t') -> gClojure g t'
    (Seq l r)           -> g l ++ gClojure g r
    Eps                 -> ""

gTimes       :: Int -> (ParseTree -> String) -> ParseTree -> String
gTimes 0 _ _ = ""
gTimes 1 g t = g t
gTimes n g t = case t of
    (Rule "Repetition" t') -> gTimes n g t'
    (Seq l r)              -> g l ++ gTimes (n - 1) g r

-- Aliases --
gStar = gClojure
