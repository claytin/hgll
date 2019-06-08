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
import Parser.Generator.Util (gTerm, gStar)

gLetter (Rule "Letter" t) = gTerm t

gDecimalDigit (Rule "DecimalDigit" t) = gTerm t

gConcatenateSymbol (Rule "ConcatenateSymbol" t) = "#"
gDefiningSymbol    (Rule "DefiningSymbol" t)    = gTerm t
gAlternativeSymbol (Rule "AlternativeSymbol" t) = "<|>"

gStartGroupSymbol  (Rule "StartGroupSymbol" t)  = gTerm t
gStartOptionSymbol (Rule "StartOptionSymbol" t) = "("
gStartRepeatSymbol (Rule "StartRepeatSymbol" t) = "("

gEndGroupSymbol  (Rule "EndGroupSymbol" t)  = gTerm t
gEndOptionSymbol (Rule "EndOptionSymbol" t) = ")"
gEndRepeatSymbol (Rule "EndRepeatSymbol" t) = ")"

gSingleQuoteSymbol (Rule "SingleQuoteSymbol" t) = "\""
gDoubleQuoteSymbol (Rule "DoubleQuoteSymbol" t) = gTerm t

gRepetitionSymbol (Rule "RepetitionSymbol" t) = "*."

gTerminatorSymbol (Rule "TerminatorSymbol" t) = ""

gOtherCharacter (Rule "OtherCharacter" t) = case t of
    (Rule _ _) -> gSpaceCharacter t
    _          -> gTerm t

gSpaceCharacter (Rule "SpaceCharacter" t) = gTerm t

gNewLine (Rule "NewLine" t) = case t of
    (Seq (Seq l m) r) -> gStar gCarriageReturn l
                      ++ gTerm m
                      ++ gStar gCarriageReturn r

gCarriageReturn (Rule "CarriageReturn" t) = gTerm t

gHorizontalTabulationCharacter (Rule "HorizontalTabulationCharacter" t) =
    gTerm t
gVerticalTabulationCharacter   (Rule "VerticalTabulationCharacter" t) =
    gTerm t

gFormFeed (Rule "FormFeed" t) = gTerm t
