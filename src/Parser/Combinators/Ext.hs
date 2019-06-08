module Parser.Combinators.Ext ( module Parser.Combinators.Base
                              , rule', (=!>)
                              , opt
                              , clojure, star
                              , times, (*.) ) where

import Parser.Instance.Std
import Parser.Data.ParseTree

import Parser.Combinators.Base

-- The rule' is a firs match variation of rule. It is quite convenient on the
-- definition of certain patterns/combinators such as the ones bellow, where
-- an ambiguous combinator suchs as rule will return every intermidiate result
-- of the parser application, when we are only interested in the longest match,
-- which happens to be the first element of the list of results
rule'        :: Label -> Std ParseTree -> Std ParseTree
rule' l alts = Std $ \i ->
    case parse (rule l alts) i of
        [ ]    -> [ ]
        (x:xs) -> [x]

-- A parser that always succeeds, either by the success of its parameter p, or
-- by the empty production. This represents a sequence of at most one match of
-- the pattern described by p
opt   :: Std ParseTree -> Std ParseTree
opt p = "Optional" =!> p <|> eps

-- Similar to opt, except that clojure represents a unbount sequence of matches
-- of the pattern described by p
clojure   :: Std ParseTree -> Std ParseTree
clojure p = "Clojure" =!> p' <|> eps
    where
        p' = p # clojure p

-- The times combinator defines a finite sequence, of exactly n, matches of the
-- pattern described by p
times     :: Int -> Std ParseTree -> Std ParseTree
times 0 _ = eps
times 1 p = p
times n p = "Repetition" =!> p'
    where
        p' = p # (n - 1) *. p

-- Aliases --
star = clojure

-- The highest precedence on the set of EBNF operators
infixl 4 *.
(*.) = times

-- See the rule combinator fixity definition in Parser.Combinators.Base
infix 1 =!>
(=!>) = rule'
