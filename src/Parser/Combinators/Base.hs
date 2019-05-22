-- TODO
-- 1. Maybe add some type aliases, it is getting hard to cope with the type
-- signatures;
-- 2. Fix the comment for the Parser type;
-- 3. Maybe the memo function should be separated from the combinators.
module Parser.Combinators.Base ( Parser
                               , Input
                               , Res
                               , parse
                               , success
                               , failure
                               , eps
                               , term, t
                               , bind
                               , sqnc, (+>)
                               , rule ) where

import Parser.Data.ParseTree
import Parser.Data.Trie

-- About the Parser type.
-- Let us first look only at the value contructor. If we want to memoize the
-- result of the application of a parser we "must" pass this structure arround.
-- One way to achieve this is by definig a parser as follows:
--     Parser ((Input, Memo a) -> (Res a, Memo a))
-- that is, a function that takes an input and a memo in the form of a pair and
-- returns the result of parsing the input and the updated memo, also in the
-- form of a pair. Now forget the parametrization of Ras and Memo, for an
-- instant. Say, for example, that Input is a, Memo is m, and res is b. We can
-- rewrite the parser definition as:
--    Parser ((a, m) -> (b, m))
-- we can say that a parser is a function from that transforms some input of
-- type into some value of type b, while updating some state m.
-- We can dismantle the first parameter of Parser, the pair (a, m), and rewrite
-- Parser:
--    Parser (a -> m -> (b, m))
-- this is the same parser as before, only now the elements of the first
-- parameter of a parser are each a parameter. Furthermore, since the operator
-- (->) is right-associative, we have
--     Parser (a -> (m -> (b, m)))
-- where the function m -> (b, m) is called a "state modifying function".
-- "Separating" this function from the Parser type definition we can define
--     State m b = State (m -> (b, m))
-- substitutind this in the Parser definition we have
--     Parser a m b = Parser (a -> State m b)
-- Now if we replace a, b, and m, by the types used in the first definition of
-- Parser, we have
--     Parser (Input -> State (Memo a) (Res a))
-- Since the types parameters of Memo and Res must be the same, since the memo
-- ought to store the values resulting from the application of a combinator, we
-- can remove the previous parametrization of the type constructor Parser a m b
--     Parser a = Parser (Input -> State (Memo a) (Res a))
newtype Parser a r = Parser (Input -> (Cont (Res a) r -> r))
--newtype Parser a = Parser (Input -> Memo (Res a) -> (Res a, Memo (Res a)))
-- where
type Memo a = Trie a
type Input  = String
-- The result of applying a parser is Either, in case of success, a pair
-- composed by a value of type a (in this implementation mostly a ParseTree),
-- and the remainder of the input unconsuned by the parser. In case of failure
-- the result is the input that the parser failed to consume
type Res a = Either Input      -- failure
                    (a, Input) -- success

type Cont a r = a -> r

-- The parse function deconstructs and applies a parser
--parse            :: Parser a -> (Input -> Memo (Res a) -> (Res a, Memo (Res a)))
parse            :: Parser a r -> (Input -> (Cont (Res a) r -> r))
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
success   :: a -> Parser a r
-- success v = memo $ Parser $ \i m -> (Right (v, i), m)
success v = Parser $ \i ->
    \k -> k $ Right (v, i)

-- The complement of success, it always fails
failure :: Parser a r
failure = Parser $ \i ->
    \k -> k $ Left i
--failure = memo $ Parser $ \i m -> (Left i, m)

-- Parser that represents an empty production. The parser always succeeds with
-- an "empty" ParseTree (see Parser.Data.ParseTree)
eps :: Parser ParseTree r
eps = success Eps

-- This is the only parser for matching terminals. It takes a string s that
-- will be matched against the first n characters of input i, where n is the
-- length of the parameter s. In case of match the Token s (or s') is returned.
-- Unlike most common parser combinators no parametrization of the token
-- content (value) is allowed. ISO EBNF standart defines strings as the only
-- terminal elements, therefore is not possible to distinguish types on a
-- grammar definition
term    :: String -> Parser ParseTree r
term "" = error "Terminals must be non empty strings!"
--term s  = memo $ Parser $ \i m ->
--    let n  = length s
--        s' = take n i
--        i' = drop n i
--    in if s == s' then (Right (Token s', i'), m)
--                  else (Left i, m)
--
term s  = Parser $ \i ->
    \k -> let n  = length s
              s' = take n i
              i' = drop n i
           in if s == s' then k $ Right (Token s', i')
                         else k $ Left i
--
---- Altough it returns a parser, i see this as more of a auxilarie function,
---- very convinient for the implementation of sequencies and recursive patterns.
---- It takes a parser and a function that returns a parser. The function f takes
---- the value returned by the parser p and carries it (binds) forward into its
---- "scope". If any of the parsers, p or the one returned by f, fail, then bind
---- also fails
--bind     :: Parser a r -> (a -> Parser a r) -> Parser a r
--bind p f = memo $ Parser $ \i m ->
--    case parse p i m of
--        (Right (v, i'), m') -> parse (f v) i' m'
--        _                   -> (Left i, m)
--
bind p f = Parser $ \i ->
    \k -> case parse p i k of
        Right (v, i') -> parse (f v) i' k
        _             -> k $ Left i
--
---- The sequence combinator is a function that takes two parsers and apply both
---- of them, in order, from the left, to the given input by way of the bind
---- "parser".
--sqnc     :: Parser ParseTree r -> Parser ParseTree r -> Parser ParseTree r
sqnc p q = p `bind` \x ->
           q `bind` \y ->
           success $ Seq x y
--
---- A helper function that takes the result of the application of a parser and
---- "encapsulates" it into a new result. This new result implies no change to
---- the result of the computation. If the original result was a failure, label
---- behaves like the identity function. Otherwise, it will take the resulting
---- parse tree and "put it inside" a new Rule parse tree labeled by l. Every
---- element of a grammar (except maybe for the start rule) must be an element
---- of some rule, the label function just names what rule it is
----label                     :: Label
----                          -> (Res ParseTree, Memo ParseTree)
----                          -> (Res ParseTree, Memo ParseTree)
--label _ l@(Left _, _)     = l
--label l (Right (t, i), m) = (Right (Rule l t, i), m)
label _ l@(Left _)     = l
label l (Right (t, i)) = Right (Rule l t, i)
--
--
---- A first match alternative combinator, the first of the alternatives to
---- succeed is returned. It fails if none of the alternatives can parse the
---- input
rule        :: Label -> [Parser a (Res ParseTree)] -> Parser a (Res ParseTree)
rule l []   = failure -- a rule must not be empty
--rule l alts = memo $ Parser $ \i m -> label l $ parse (each alts) i m
--    where
--        -- each reduce the list of parsers by meas of orElse
--        each = foldl1 orElse
--        -- orElse takes two parsers at a time, if the (cur)rent one succeeds it
--        -- returns the result, or else try the nxt one
--        cur `orElse` nxt = Parser $ \i m ->
--            case parse cur i m of
--                r@(Right _, _) -> r
--                _              -> parse nxt i m
--
rule l alts = Parser $ \i ->
    \k -> label l $ parse (each alts) i k
        where
            -- each reduce the list of parsers by meas of orElse
            each = foldl1 orElse
            -- orElse takes two parsers at a time, if the (cur)rent one succeeds it
            -- returns the result, or else try the nxt one
            cur `orElse` nxt = Parser $ \i ->
                \cont -> parse cur i $ \r -> case r of
                    Right _ -> cont r
                    _       -> parse nxt i cont
--
---- Aliases --
t = term
--
---- If (+>) is applied to a sequence of, say, elements a, b, and c a +> b +> c
---- is equivalent to (a +> b) +> c
infixl 1 +>
(+>) = sqnc
