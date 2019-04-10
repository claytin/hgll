module Parser.Combinators.Base ( Parser
                               , ParseRes
                               , Input
                               , Label -- defined by Parser.Data.ParseTree
                               , eps
                               , term, t
                               , sqnc, s
                               , alt, (<|>) ) where

import Parser.Data.ParseTree

-- The result of applying a parser is either a Success, for which is given the
-- correspondent ParseTree and the remainder of the input, or it is a Failure,
-- for which we return the unmodified input
type ParseRes = [(ParseTree, Input)]
-- where
type Input = String

-- A parser is a fuction that takes a memo, some data structure that holds
-- computed values, a string, and returns a ParseRes
type Parser = Input -> ParseRes

{-- Combinators --}
-- Parser that represents an empty production. The parser always succeeds with
-- an "empty" ParseTree (see Parser.Data.Syntax)
eps :: Parser
eps = \i -> [(Eps, i)]

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
    in if s == s' then [(Leaf s', i')]
       else            []

-- The sequence combinator takes a list of parsers and apply each of them, in
-- order, to the given input. For each successful match the resulting ParseTree
-- is stored as one of the children of the sequence, if all parsers succeed the
-- correpondent sub-tree is retuned, otherwise it fails without modifiyng the
-- original input, there is no partial parsing of a sequence
sqnc      :: Label -> [Parser] -> Parser
sqnc l [] = error $ "Sequence " ++ l ++ " must not be empty!"
sqnc l ps = \i -> sqnc' ps i []
    -- reverse is used because the parse trees are being prepended into acc
    where sqnc' []     inp acc = [(Node l $ reverse acc, inp)]
          sqnc' (q:qs) inp acc = case q inp of
              []          -> []
              [(t, inp')] -> sqnc' qs inp' (t:acc)

-- A first match alternative combinator, if the first of the combinators (p)
-- succeed then its result is returned, otherwise we try to match q
alt     :: Parser -> Parser -> Parser
alt p q = \i -> p i ++ q i

{-- Aliases --}
t = term
s = sqnc

infixl 2 <|>
(<|>) = alt
