module Parser.Generator.CharSet ( gLetter
                                , gDecimalDigit
                                , gConcatenateSymbol
                                , gDefiningSymbol
                                , gAlternativeSymbol
                                , gEndCommentSymbol
                                , gEndGroupSymbol
                                , gEndOptionSymbol
                                , gEndRepeatSymbol
                                , gFirstQuoteSymbol
                                , gRepetitionSymbol
                                , gSecondQuoteSymbol
                                , gExceptSymbol
                                , gSpecialSequenceSymbol
                                , gStartCommentSymbol
                                , gStartGroupSymbol
                                , gStartOptionSymbol
                                , gStartRepeatSymbol
                                , gTerminatorSymbol
                                , gOtherCharacter ) where

import Parser.Data.ParseTree

import Parser.Generator.Util (gTerm, gClosure, gEscQuoteS)

gLetter (Rule "Letter" t) = gTerm t

gDecimalDigit (Rule "DecimalDigit" t) = gTerm t

gConcatenateSymbol     (Rule "ConcatenateSymbol" t)     = gTerm t
gDefiningSymbol        (Rule "DefiningSymbol" t)        = gTerm t
gAlternativeSymbol     (Rule "AlternativeSymbol" t)     = gTerm t
gEndCommentSymbol      (Rule "EndCommentSymbol" t)      = gTerm t
gEndGroupSymbol        (Rule "EndGroupSymbol" t)        = gTerm t
gEndOptionSymbol       (Rule "EndOptionSymbol" t)       = gTerm t
gEndRepeatSymbol       (Rule "EndRepeatSymbol" t)       = gTerm t
gFirstQuoteSymbol      (Rule "FirstQuoteSymbol" t)      = gTerm t
gRepetitionSymbol      (Rule "RepetitionSymbol" t)      = gTerm t
gSecondQuoteSymbol     (Rule "SecondQuoteSymbol" _)     = gEscQuoteS
gExceptSymbol          (Rule "ExceptSymbol" t)          = gTerm t
gSpecialSequenceSymbol (Rule "SpecialSequenceSymbol" t) = gTerm t
gStartCommentSymbol    (Rule "StartCommentSymbol" t)    = gTerm t
gStartGroupSymbol      (Rule "StartGroupSymbol" t)      = gTerm t
gStartOptionSymbol     (Rule "StartOptionSymbol" t)     = gTerm t
gStartRepeatSymbol     (Rule "StartRepeatSymbol" t)     = gTerm t
gTerminatorSymbol      (Rule "TerminatorSymbol" t)      = gTerm t

gOtherCharacter (Rule "OtherCharacter" t) = case t of
    (Rule _ _) -> gSpaceCharacter t
    _          -> gTerm t

gSpaceCharacter (Rule "SpaceCharacter" t) = gTerm t

gNewLine (Rule "NewLine" t) = case t of
    (Seq (Seq l m) r) -> gClosure gCarriageReturn l
                      ++ gTerm m
                      ++ gClosure gCarriageReturn r

gCarriageReturn (Rule "CarriageReturn" t) = gTerm t

gHorizontalTabulationCharacter (Rule "HorizontalTabulationCharacter" t) =
    gTerm t
gVerticalTabulationCharacter   (Rule "VerticalTabulationCharacter" t) =
    gTerm t

gFormFeed (Rule "FormFeed" t) = gTerm t
