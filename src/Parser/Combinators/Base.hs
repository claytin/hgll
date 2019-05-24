-- TODO!
-- 1. Maybe add some more type aliases, it is getting hard to cope with the
-- type signatures;
-- 2. Add new comment for the Parser type;
-- 3. Maybe the memo function should be separated from the combinators;
-- 4. Remove commented code
module Parser.Combinators.Base ( Parser(..)
                               , Input
                               , parse
                               , success
                               , failure
                               , eps
                               , term, t
                               , sqnc, (+>)
                               , rule ) where

import Parser.Data.ParseTree

-- About the Parser type.
-- Add comment here ...
newtype Parser a = Parser (Input -> [(a, Input)])
--newtype Parser a = Parser (Input -> Memo (Res a) -> (Res a, Memo (Res a)))
-- where
--type Memo a = Trie a
type Input  = String

-- The parse function deconstructs and applies a parser
--parse            :: Parser a -> (Input -> Memo (Res a) -> (Res a, Memo (Res a)))
parse            :: Parser a -> (Input -> [(a, Input)])
parse (Parser p) = p

--memo :: Parser a -> Parser a
--memo p = Parser $ \i m ->
--    case fetch i m of
--        Just v -> (v, m)
--        _      -> let (v, m') = parse p i m
--                      m''     = add i v m'
--                   in (v, m'')

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
--term s  = memo $ Parser $ \i m ->
--    let n  = length s
--        s' = take n i
--        i' = drop n i
--    in if s == s' then (Right (Token s', i'), m)
--                  else (Left i, m)

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
--label _ l@(Left _, _)     = l
--label l (Right (t, i), m) = (Right (Rule l t, i), m)

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
