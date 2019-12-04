{-- Defines the combinators syntax for parser generation --}
module Parser.Generator.Util.C ( gEpsC
                               , gTermC
                               , gAltC
                               , gSqncC
                               , gRuleC
                               , gOptC
                               , gClosureC
                               , gRepC
                               , gExceptC ) where

import Parser.Data.ParseTree

gEpsC :: String
gEpsC = "eps"

gTermC :: String
gTermC = "t "

gAltC (Rule "AlternativeSymbol" _) = " <|> "

gSqncC (Rule "ConcatenateSymbol" _) = " # "

gRuleC (Rule "DefiningSymbol" _) = " =|> "

gOptC :: String
gOptC = "opt "

gClosureC :: String
gClosureC = "closure "

gRepC (Rule "RepetitionSymbol" _) = " *. "

gExceptC (Rule "ExceptSymbol" _) = " -. "
