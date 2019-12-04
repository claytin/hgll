module Parser.Generator.Util ( gTerm
                             , gOptional
                             , gClosure
                             , gRepetition
                             , module Parser.Generator.Util.C
                             , module Parser.Generator.Util.S ) where

import Parser.Data.ParseTree

import Parser.Generator.Util.C
import Parser.Generator.Util.S

-- This is an auxilary function for extracting the terminal string tk from the
-- terminal tree Token. The function gTerm is only used to generate terminals,
-- meaning only elements of final strings;
gTerm :: ParseTree -> String
gTerm (Token tk) = tk

-- The first equation in the following functions (Rule ...) unpacks the actual
-- tree structure and is necessary because of the recursive pattern described
-- by them;
-- For a better understanding of how these functions work and of their pattern
-- matching see Parser/Combinators/Ext*.hs
gOptional     :: (ParseTree -> String) -> ParseTree -> String
gOptional g t = case t of
    (Rule "Optional" t') -> gOptional g t'
    -- this comment creates separation from the above "dummy" equation
    Eps -> gEpsS
    _   -> g t

gClosure     :: (ParseTree -> String) -> ParseTree -> String
gClosure g t = case t of
    (Rule "Closure" t') -> gClosure g t'
    --
    (Seq l r) -> g l ++ gClosure g r
    Eps       -> gEpsS

gRepetition       :: Int -> (ParseTree -> String) -> ParseTree -> String
gRepetition 0 _ t = gEpsS
gRepetition n g t = case t of
    (Rule "Repetition" t') -> gRepetition n g t'
    --
    (Seq l r) -> g l ++ gRepetition (n - 1) g r
