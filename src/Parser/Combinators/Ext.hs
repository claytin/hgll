module Parser.Combinators.Ext ( opt
                              , clojure, star
                              , times, (*.) ) where

import Parser.Combinators.Base
import Parser.Data.ParseTree

opt   :: Parser ParseTree -> Parser ParseTree
opt p = rule "Optional" [ p , eps ]

clojure   :: Parser ParseTree -> Parser ParseTree
clojure p = rule "Clojure" [ p', eps ]
    where
        p' = p         `bind` \x ->
             clojure p `bind` \y ->
             success $ Seq x y

times     :: Int -> Parser ParseTree -> Parser ParseTree
times 0 _ = eps
times 1 p = p
times n p = rule "Repetition" [ p' ]
    where
        p' = p `bind`                 \x ->
             (n - 1) `times` p `bind` \y ->
             success $ Seq x y

-- Aliases
star = clojure

infixl 0 *.  -- this may have to change
(*.) = times
