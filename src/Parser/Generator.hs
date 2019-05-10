module Parser.Generator ( gen ) where

import Parser.Combinators.Base (parse)

import Parser.Generator.Syntax
import Parser.EBNF.Syntax

gen   :: String -> Either String String
gen i = case parse syntax i of
    Right (t, remainder) ->
        if length remainder > 0 then
            Right $ gSyntax t
        else
            Left $ "Failed with a partial generation!\n"
                ++ "Could not generate parsers for the following input:\n"
                ++ remainder
    Left i' -> Left $ "Parser failed at input:\n" ++ i'
