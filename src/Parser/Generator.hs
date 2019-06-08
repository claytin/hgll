module Parser.Generator ( gen ) where

import Parser.Traits

import Parser.EBNF.Syntax
import Parser.Generator.Syntax

gen   :: String -> Maybe String
gen i = case fullParse syntax i of
    [ ] -> Nothing
    [t] -> Just $ gSyntax t
