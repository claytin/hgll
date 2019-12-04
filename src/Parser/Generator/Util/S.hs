{-- Defines some symbols and special uses cases of them --}
module Parser.Generator.Util.S ( gEpsS
                               , gSepS
                               , gQuoteS
                               , gEscQuoteS
                               , gFirstQuoteS
                               , gSecondQuoteS
                               , gOpenParnS
                               , gCloseParnS
                               , gTerminatorS ) where

import Parser.Data.ParseTree

gEpsS :: String
gEpsS = ""

gSepS :: String
gSepS = " "

gQuoteS :: String
gQuoteS = "\""

-- Quotes must be escaped for terminal combinators to work, this is used
-- instead of gterm, when generating SecondQuoteSymbol
gEscQuoteS :: String
gEscQuoteS = "\\\""

-- The following substitute g*QuoteSymbol in Parser/Generator/Words.hs;
-- When generating a string literal the FirstQuoteSymbol cannot be used, since
-- Haskell's strings are delimited by double quotes;
gFirstQuoteS  (Rule "FirstQuoteSymbol" _)  = gQuoteS
-- SecondQuoteSymbol defines a special symbol as well as a valid terminal
-- character within an ISO EBNF terminal. However, for the purpose of
-- combinators generation, these are different cases;
gSecondQuoteS (Rule "SecondQuoteSymbol" _) = gQuoteS

gOpenParnS :: String
gOpenParnS = "("

gCloseParnS :: String
gCloseParnS = ")"

gTerminatorS (Rule "TerminatorSymbol" _) = "\n"
