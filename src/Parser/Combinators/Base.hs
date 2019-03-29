-- TODO
-- Some of the comments for the implementation of some of the combinators do
-- not match their implementation
module Parser.Combinators.Base ( Parser
                               , ParseRes(..)
                               , Input
                               , eps
                               , term, t
                               , sqnc, s
                               , alt, (>|<)) where

import Parser.Data.ParseTree

-- The result of applying a parser is either a Success, for which is given the
-- correspondent ParseTree and the remainder of the input, or it is a Failure,
-- for which we return the unmodified input
data ParseRes = Success ParseTree String
              | Failure String

{-- Combinators --}
-- A parser is a fuction that takes a memo, some data structure that holds
-- computed values, a string, and returns a ParseRes
type Parser = Input -> ParseRes -- where
type Input  = String

-- Parser that represents an empty production. The parser always succeeds with
-- an "empty" ParseTree (see Parser.Data.Syntax)
eps :: Parser
eps = \i -> Success Eps i

-- Since iso EBNF terminals are strings and that this implementation have no
-- parametrization of its parsers, this is the only parser for matching
-- terminals. It takes a string s of size n that will be matched against the
-- first n characters of input i, represented by s'
term    :: String -> Parser
term "" = error "Terminals must be non empty strings!"
term s  = \i ->
    let n  = length s
        s' = take n i
        i' = drop n i
    in if s == s' then Success (Leaf s') i'
       else            Failure i

-- The sequence combinator takes a list of parsers and apply each of them, in
-- order, to the given input. For each successful match the resulting ParseTree
-- is stored as one of the children of the sequence, if all parsers succeed the
-- correpondent sub-tree is retuned, otherwise it fails without modifiyng the
-- original input, there is no partial parsing of a sequence
sqnc      :: Label -> [Parser] -> Parser
sqnc l [] = error $ "Sequence " ++ l ++ " is empty!"
sqnc l ps = \i ->
    let sqnc' [] inp acc     = (acc, inp)
        sqnc' (q:qs) inp acc = case q inp of
            (Success t r) -> sqnc' qs r (t:acc)
            _             -> ([], inp)
    in case sqnc' ps i [] of
            ([], r) -> Failure r
            (cs, r) -> Success (Node l (reverse cs)) r

-- A first match alternative combinator, if the first of the combinators (p)
-- succeed then its result is returned, otherwise we try to match q
alt     :: Parser -> Parser -> Parser
alt p q = \i -> case p i of
    v@(Success _ _) -> v
    _               -> q i

-- Aliases
t = term
s = sqnc

-- a >|< b >|< ... >|< z = a >|< (b >|< (... >|< z))
infixr 2 >|< -- should this be left associative?
(>|<) = alt

instance Show ParseRes where
    show (Success t r) = "Success _:" ++ show r ++ newLine
                    ++ newLine
                    ++ show t
                       where newLine = "\n"
    show (Failure r)   = "Failure " ++ show r
