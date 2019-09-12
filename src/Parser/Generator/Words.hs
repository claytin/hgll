module Parser.Generator.Words ( gTerminalString
                              , gMetaIdentifier
                              , gInteger ) where

import Parser.Data.ParseTree

import Parser.Generator.Util
import Parser.Generator.CharSet

gTerminalString (Rule "TerminalString" t) = case t of
    (Seq (Seq (Seq (Rule "FirstQuoteSymbol" _) b) c) _) ->
        cTermOp ++ cQuoteSym
                ++ gFirstTerminalCharacter b
                ++ gStar gFirstTerminalCharacter c
                ++ cQuoteSym
    (Seq (Seq (Seq (Rule "SecondQuoteSymbol" _) b) c) _) ->
        cTermOp ++ cQuoteSym
                ++ gSecondTerminalCharacter b
                ++ gStar gSecondTerminalCharacter c
                ++ cQuoteSym

gFirstTerminalCharacter (Rule "FirstTerminalCharacter" t) = case t of
    (Rule "Letter" _)                -> gLetter t
    (Rule "DecimalDigit" _)          -> gDecimalDigit t
    (Rule "ConcatenateSymbol" _)     -> gConcatenateSymbol t
    (Rule "DefiningSymbol" _)        -> gDefiningSymbol t
    (Rule "AlternativeSymbol" _)     -> gAlternativeSymbol t
    (Rule "EndGroupSymbol" _)        -> gEndGroupSymbol t
    (Rule "EndOptionSymbol" _)       -> gEndOptionSymbol t
    (Rule "EndRepeatSymbol" _)       -> gEndRepeatSymbol t
    (Rule "ExceptSymbol" _)          -> gExceptSymbol t
    (Rule "RepetitionSymbol" _)      -> gRepetitionSymbol t
    (Rule "SecondQuoteSymbol" _)     -> gSecondQuoteSymbol t
    (Rule "SpecialSequenceSymbol" _) -> gSpecialSequenceSymbol t
    (Rule "StartGroupSymbol" _)      -> gStartGroupSymbol t
    (Rule "StartOptionSymbol" _)     -> gStartOptionSymbol t
    (Rule "StartRepeatSymbol" _)     -> gStartRepeatSymbol t
    (Rule "TerminatorSymbol" _)      -> gTerminatorSymbol t
    (Rule "OtherCharacter" _)        -> gOtherCharacter t

gSecondTerminalCharacter (Rule "SecondTerminalCharacter" t) = case t of
    (Rule "Letter" _)                -> gLetter t
    (Rule "DecimalDigit" _)          -> gDecimalDigit t
    (Rule "ConcatenateSymbol" _)     -> gConcatenateSymbol t
    (Rule "DefiningSymbol" _)        -> gDefiningSymbol t
    (Rule "AlternativeSymbol" _)     -> gAlternativeSymbol t
    (Rule "EndGroupSymbol" _)        -> gEndGroupSymbol t
    (Rule "EndOptionSymbol" _)       -> gEndOptionSymbol t
    (Rule "EndRepeatSymbol" _)       -> gEndRepeatSymbol t
    (Rule "ExceptSymbol" _)          -> gExceptSymbol t
    (Rule "FirstQuoteSymbol" _)      -> gFirstQuoteSymbol t
    (Rule "RepetitionSymbol" _)      -> gRepetitionSymbol t
    (Rule "SpecialSequenceSymbol" _) -> gSpecialSequenceSymbol t
    (Rule "StartGroupSymbol" _)      -> gStartGroupSymbol t
    (Rule "StartOptionSymbol" _)     -> gStartOptionSymbol t
    (Rule "StartRepeatSymbol" _)     -> gStartRepeatSymbol t
    (Rule "TerminatorSymbol" _)      -> gTerminatorSymbol t
    (Rule "OtherCharacter" _)        -> gOtherCharacter t

gMetaIdentifier (Rule "MetaIdentifier" t) = case t of
    (Seq l r) -> gLetter l ++ gStar gMetaIdentifierCharacter r

gMetaIdentifierCharacter (Rule "MetaIdentifierCharacter" t) = case t of
    (Rule "Letter" _) -> gLetter t
    _                 -> gDecimalDigit t

gInteger (Rule "Integer" t) = case t of
    (Seq l r) -> gDecimalDigit l ++ gStar gDecimalDigit r
