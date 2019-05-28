-- TODO!
-- 1. Add some comments
module Parser.Combinators.Ext ( module Parser.Combinators.Base
                              , rule', (=!>)
                              , opt
                              , clojure, star
                              , times, (*.) ) where

import Parser.Types
import Parser.Combinators.Base

rule'        :: Label -> [Parser ParseTree] -> Parser ParseTree
rule' l []   = failure -- a rule must not be empty
rule' l alts = Parser $ \i ->
    case parse (rule l alts) i of
        [ ]    -> [ ]
        (x:xs) -> [x]

opt   :: Parser ParseTree -> Parser ParseTree
opt p = rule' "Optional" [ p , eps ]

clojure   :: Parser ParseTree -> Parser ParseTree
clojure p = rule' "Clojure" [ p', eps ]
    where
        p' = p +> clojure p

times     :: Int -> Parser ParseTree -> Parser ParseTree
times 0 _ = eps
times 1 p = p
times n p = rule' "Repetition" [ p' ]
    where
        p' = p +> (n - 1) `times` p

-- Aliases --
star = clojure

-- The precedence is set to zero beacause the precedence of (+>, sequence) is
-- set to one, repetition must have a higher precedence
infixl 2 *.
(*.) = times

-- See rule fixity definition in Parser.Combinators.Base
infix 9 =!>
(=!>) = rule'
