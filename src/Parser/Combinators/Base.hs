module Parser.Combinators.Base ( ParserRes(..)
                               , Parser
                               , succeed
                               , failp
                               , term, t
                               , sqnc, s
                               , alt, (>|<) ) where

import Parser.Data.Syntax

-- The result of applying a parser is either a Success, for which is given the
-- correspondent ParseTree and the remainder of the input, or it is a Failure,
-- for which we return the unmodified input
data ParserRes = Success ParseTree String
               | Failure String

instance Show ParserRes where
    show (Success t s) = "Success _:" ++ show s ++ newLine
                      ++ newLine
                      ++ show t
                         where newLine = "\n"
    show (Failure s)   = "Failure " ++ show s

-- For now a parser is a fuction that takes a string and returns a ParserRes
type Parser =  String -> ParserRes

-- This is a parser that always succeeds, it returns a sigleton list for a
-- given ParseTree without modifying its input string
succeed   :: ParseTree -> Parser
succeed t = \i -> Success t i

-- Parser that represents an empty production. The parser always succeeds with
-- an "empty" ParseTree (see Parser.Data.Syntax)
eps :: Parser
eps = succeed Eps

-- A parser that fails regardless of its input
failp :: Parser
failp = \i -> Failure i

-- Since iso EBNF terminals are strings and that this implementation have no
-- parametrization of its parsers, this is the only parser for matching
-- terminals. It takes a string s of size n that will be matched against the
-- first n characters of input i, represented by s'
term    :: String -> Parser
term "" = error "Err: tokens must be non empty strings!"
term s  = \i ->
    let n  = length s
        s' = take n i
        i' = drop n i
    in if s == s' then succeed (Leaf s') i'
       else            failp i

-- The sequence combinator takes a list of parsers and apply each of them, in
-- order, to the given input. For each successful match the resulting ParseTree
-- is stored as one of the children of the sequence, if all parsers succeed the
-- correpondent sub-tree is retuned, otherwise it fails without modifiyng the
-- original input, there is no partial parsing of a sequence
sqnc      :: Label -> [Parser] -> Parser
sqnc l [] = error $ "Err: sequence " ++ l ++ " must not be empty!"
sqnc l ps = \i -> case sqncR [] ps i of
    ([], _)  -> failp i -- this case first, because of overlapping
    -- reverse is used because each child of l is prepended in acc (sqncR),
    -- therefore the order they apear in cs is the reverse of their matching
    (cs, i') -> succeed (Node l (reverse cs)) i'
    -- sqncR stands for the sequence recursion (recursive application)
    where sqncR acc []     inp = (acc, inp)
          sqncR acc (q:qs) inp = case q inp of
              (Success t' inp') -> sqncR (t':acc) qs inp' -- prepend child
              (Failure inp')    -> ([], inp')

-- A first match alternative combinator, if the first of the combinators (p)
-- succeed then its result is returned, otherwise we try to match q
alt     :: Parser -> Parser -> Parser
alt p q = \i -> case p i of
    (Failure _) -> q i
    s           -> s

-- Aliases
t = term
s = sqnc

-- a >|< b >|< ... >|< z = a >|< (b >|< (... >|< z))
infixr 2 >|<
(>|<) = alt
