module Parser.Generator.CharSet ( gLetter
                                , gDecimalDigit
                                , gConcatenateSymbol
                                , gDefiningSymbol
                                , gAlternativeSymbol
                                , gStartGroupSymbol
                                , gStartOptionSymbol
                                , gStartRepeatSymbol
                                , gEndGroupSymbol
                                , gEndOptionSymbol
                                , gEndRepeatSymbol
                                , gSingleQuoteSymbol
                                , gDoubleQuoteSymbol
                                , gRepetitionSymbol
                                , gTerminatorSymbol
                                , gOtherCharacter ) where

import Parser.Data.ParseTree

-- These imports are temporary, they are for testing only (f u ghci)
import Parser.Combinators.Base
import Parser.EBNF.CharSet

unpack (Token tk) = tk

gLetter (Rule "Letter" t) = unpack t

gDecimalDigit (Rule "DecimalDigit" t) = unpack t

gConcatenateSymbol (Rule "ConcatenateSymbol" t) = "+>"
gDefiningSymbol    (Rule "DefiningSymbol" t)    = unpack t
gAlternativeSymbol (Rule "AlternativeSymbol" t) = ","

gStartGroupSymbol  (Rule "StartGroupSymbol" t)  = unpack t
gStartOptionSymbol (Rule "StartOptionSymbol" t) = "("
gStartRepeatSymbol (Rule "StartRepeatSymbol" t) = "("

gEndGroupSymbol  (Rule "EndGroupSymbol" t)  = unpack t
gEndOptionSymbol (Rule "EndOptionSymbol" t) = ")"
gEndRepeatSymbol (Rule "EndRepeatSymbol" t) = ")"

gSingleQuoteSymbol (Rule "SingleQuoteSymbol" t) = "\""
gDoubleQuoteSymbol (Rule "DoubleQuoteSymbol" t) = unpack t

gRepetitionSymbol (Rule "RepetitionSymbol" t) = ""

gTerminatorSymbol (Rule "TerminatorSymbol" t) = ""

gOtherCharacter (Rule "OtherCharacter" t) = case t of
    (Rule _ _) -> gSpaceCharacter t
    _          -> unpack t

gSpaceCharacter (Rule "SpaceCharacter" t) = unpack t
