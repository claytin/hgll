module Parser.Generator ( gen ) where

import Parser.Types
import Parser.Combinators.Base (parse)

import Parser.Generator.Syntax
import Parser.EBNF.Syntax

gen   :: String -> Either String String
gen i = case parse syntax i of
    [ ]      -> Left  $ "Parser failed. Aborting!\n"
    [(t, i)] -> Right $ gSyntax t
