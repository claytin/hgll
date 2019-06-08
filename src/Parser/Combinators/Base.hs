module Parser.Combinators.Base ( parse
                               , success
                               , failure
                               , eps
                               , term, t
                               , sqnc, (#)
                               , rule, (=|>) ) where

import Parser.Instance.Std
import Parser.Data.ParseTree

-- Standard Combinators --
-- A parser that always succeeds for a given value
success :: a -> Std a
success = return

-- The complement of success, it always fails
failure :: Std a
failure = Std $ \_ -> [ ]

-- Parser that represents an empty production. The parser always succeeds with
-- an "empty" ParseTree (see Parser.Data.ParseTree)
eps :: Std ParseTree
eps = success Eps

-- This is the only parser for matching terminals. It takes a string s that
-- will be matched against the first n characters of input i, where n is the
-- length of the parameter s. In case of match the Token s (or s') is returned.
-- Unlike most common parser combinators, no parametrization of the token
-- content (value) is allowed. ISO EBNF standard defines strings as the only
-- terminal elements
term    :: String -> Std ParseTree
term "" = error "Terminals must be non empty strings!"
term s  = Std $ \i ->
    let n  = length s
        s' = take n i
        i' = drop n i
     in if s == s' then [(i', Token s')]
                   else [ ]

-- The sequence combinator is a function that takes two parsers and apply both
-- of them, in order, from the left, to the given input
sqnc     :: Std ParseTree -> Std ParseTree -> Std ParseTree
sqnc p q = p <:> q >>= \(x, y) -> return $ Seq x y


-- The rule function is more of a wrapper function. It takes a label, which
-- will identify a set of alternatives alts. Even though alts is a single
-- parser it is expected for it to be compose by one or more parsers combined
-- by the alternative operator (<|>), see Parser.Intance.Std
rule        :: Label -> Std ParseTree -> Std ParseTree
rule l alts = Std $ \i -> label l $ parse alts i
    where
        -- The function label takes the result of the application of a parser
        -- and "encapsulates" it into a new result. If the original result was
        -- a failure, label behaves like the identity function. Otherwise, it
        -- will take the resulting parse tree t and "put it inside" a new Rule
        -- parse tree labeled by l. No actual change to the structure of t
        -- occurs
        -- label :: String -> [Res ParseTree] -> [Res ParseTree]
        label l rs = [ (i', Rule l t) | (i', t) <- rs ]

-- Aliases --
t = term

-- If (#) is applied to a sequence of, say, elements a, b, and c, a # b # c
-- is equivalent to (a # b) # c
infixl 3 #
(#) = sqnc

-- This fixity definition of rule is as follows so that it will be the last
-- element to be resolved from the definition of a grammar rule
infix 1 =|>
(=|>) = rule
