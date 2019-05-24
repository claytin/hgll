module Parser.Generator.Util ( gTerm
                             , gOpt
                             , gClojure, gStar
                             , gTimes ) where

import Parser.Data.ParseTree

gTerm            :: ParseTree -> String
gTerm (Token tk) = tk

-- The first equation of the following functions unpack the top level rule and
-- it is necessary because of the recursive repetition pattern describe by them
gOpt                       :: (ParseTree -> String) -> ParseTree -> String
gOpt g (Rule "Optional" t) = case t of
    Eps -> ""
    _   -> g t

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
