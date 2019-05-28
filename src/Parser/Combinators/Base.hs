module Parser.Combinators.Base ( parse
                               , success
                               , failure
                               , eps
                               , term, t
                               , sqnc, (+>)
                               , rule, (=|>) ) where

import Parser.Types

-- The parse function deconstructs and applies a parser
parse            :: Parser a -> (Input -> [Res a])
parse (Parser p) = p

-- Combinators --
-- A parser that always succeeds for a given value
success   :: a -> Parser a
-- success v = memo $ Parser $ \i m -> (Right (v, i), m)
success v = Parser $ \i -> [(v, i)]

-- The complement of success, it always fails
failure :: Parser a
failure = Parser $ \i -> [ ]
--failure = memo $ Parser $ \i m -> (Left i, m)

-- Parser that represents an empty production. The parser always succeeds with
-- an "empty" ParseTree (see Parser.Data.ParseTree)
eps :: Parser ParseTree
eps = success Eps

-- This is the only parser for matching terminals. It takes a string s that
-- will be matched against the first n characters of input i, where n is the
-- length of the parameter s. In case of match the Token s (or s') is returned.
-- Unlike most common parser combinators no parametrization of the token
-- content (value) is allowed. ISO EBNF standart defines strings as the only
-- terminal elements, therefore is not possible to distinguish types on a
-- grammar definition
term    :: String -> Parser ParseTree
term "" = error "Terminals must be non empty strings!"
term s  = Parser $ \i ->
    let n  = length s
        s' = take n i
        i' = drop n i
     in if s == s' then [(Token s', i')]
                   else [ ]

-- Altough it returns a parser, i see this as more of a auxilarie function,
-- very convinient for the implementation of sequencies and recursive patterns.
-- It takes a parser and a function that returns a parser. The function f takes
-- the value returned by the parser p and carries it (binds) forward into its
-- "scope". If any of the parsers, p or the one returned by f, fail, then bind
-- also fails
bind     :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \i -> concat [ parse (f v) i' | (v, i') <- parse p i ]

-- The sequence combinator is a function that takes two parsers and apply both
-- of them, in order, from the left, to the given input by way of the bind
-- "parser".
sqnc     :: Parser ParseTree -> Parser ParseTree -> Parser ParseTree
sqnc p q = p `bind` \x ->
           q `bind` \y ->
           success $ Seq x y

-- A helper function that takes the result of the application of a parser and
-- "encapsulates" it into a new result. This new result implies no change to
-- the result of the computation. If the original result was a failure, label
-- behaves like the identity function. Otherwise, it will take the resulting
-- parse tree and "put it inside" a new Rule parse tree labeled by l. Every
-- element of a grammar (except maybe for the start rule) must be an element
-- of some rule, the label function just names what rule it is
label               :: Label -> [(ParseTree, Input)] -> [(ParseTree, Input)]
label _ [ ]         = [ ]
label l ((t, i):xs) = (Rule l t, i):label l xs

-- A first match alternative combinator, the first of the alternatives to
-- succeed is returned. It fails if none of the alternatives can parse the
-- input
rule        :: Label -> [Parser ParseTree] -> Parser ParseTree
rule l []   = failure -- a rule must not be empty
rule l alts = Parser $ \i -> label l $ parse (all alts) i
    where
        -- all reduces the list of parsers by means of plus
        all = foldl1 (plus)
        -- plus takes two parsers cur and nxt, and returns a new parser that
        -- combines the application of both, cur and nxt
        cur `plus` nxt = Parser $ \i -> parse cur i ++ parse nxt i

-- Aliases --
t = term

-- If (+>) is applied to a sequence of, say, elements a, b, and c a +> b +> c
-- is equivalent to (a +> b) +> c
infixl 1 +>
(+>) = sqnc

-- This fixity definition of rule is as follows so that it stays close to
-- its function form
infix 9 =|>
(=|>) = rule
