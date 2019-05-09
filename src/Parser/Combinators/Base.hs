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

-- A Parser is a fuction from some Input string into a Res
newtype Parser a = Parser (Input -> Res a)
-- where
-- The result of applying a parser is Either, in case of success, a pair
-- composed by a value of type a (in this implementation mostly a ParseTree),
-- and the remainder of the input unconsuned by the parser. In case of failure
-- the result is the input that the parser failed to consume
type Res a = Either Input      -- failure
                    (a, Input) -- success
type Input = String

-- The parse function deconstructs and applies a parser
parse            :: Parser a -> (Input -> Res a)
parse (Parser p) = p

{-- Combinators --}

-- A parser that always succeeds for a given value
success   :: a -> Parser a
success v = Parser (\i -> Right (v, i))

-- The complement of success, it always fails
failure :: Parser a
failure = Parser (\i -> Left i)

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
    in if s == s' then Right (Token s', i')
                  else Left i

-- Altough it returns a parser, i see this as more of a auxilarie function,
-- very convinient for the implementation of sequencies and recursive patterns.
-- It takes a parser and a function that returns a parser. The function f takes
-- the value returned by the parser p and carries it (binds) forward into its
-- "scope". If any of the parsers, p or the one returned by f, fail, then bind
-- also fails
bind     :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \i ->
    case parse p i of
        Right (v, i') -> parse (f v) i'
        _             -> Left i

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
label                  :: Label -> Res ParseTree -> Res ParseTree
label _ l@(Left _)     = l
label l (Right (t, i)) = Right (Rule l t, i)

-- A first match alternative combinator, the first of the alternatives to
-- succeed is returned. It fails if none of the alternatives can parse the
-- input
rule        :: Label -> [Parser ParseTree] -> Parser ParseTree
rule l []   = failure -- a rule must not be empty
rule l alts = Parser (\i -> label l $ parse (each alts) i)
    where
        -- each reduce the list of parsers by meas of orElse
        each = foldl1 orElse
        -- orElse takes two parsers at a time, if the (cur)rent one succeeds it
        -- returns the result, or else try the nxt one
        cur `orElse` nxt = Parser $ \i ->
            case parse cur i of
                r@(Right _) -> r
                _           -> parse nxt i


{-- Combinator aliases --}
t = term

infixl 1 +>
(+>) = sqnc
