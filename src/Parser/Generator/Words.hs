module Parser.Generator.Words ( gTerminalString
                              , gMetaIdentifier
                              , gInteger ) where

import Parser.Types
import Parser.Generator.CharSet

import Parser.Generator.Util (gStar)

gTerminalString (Rule "TerminalString" t) = case t of
    (Seq (Seq (Seq a@(Rule "SingleQuoteSymbol"_) b) c) e) ->
        "t " ++ gSingleQuoteSymbol a
             ++ gFirstTerminalCharacter b
             ++ gStar gFirstTerminalCharacter c
             ++ gSingleQuoteSymbol e
    (Seq (Seq (Seq a@(Rule "DoubleQuoteSymbol"_) b) c) e) ->
        "t " ++ gDoubleQuoteSymbol a
             ++ gSecondTerminalCharacter b
             ++ gStar gSecondTerminalCharacter c
             ++ gDoubleQuoteSymbol e

gFirstTerminalCharacter (Rule "FirstTerminalCharacter" t) = case t of
    (Rule "Letter" _)            -> gLetter t
    (Rule "DecimalDigit" _)      -> gDecimalDigit t
    (Rule "ConcatenateSymbol" _) -> gConcatenateSymbol t
    (Rule "AlternativeSymbol" _) -> gAlternativeSymbol t
    (Rule "DefiningSymbol" _)    -> gDefiningSymbol t
    (Rule "StartGroupSymbol" _)  -> gStartGroupSymbol t
    (Rule "StartOptionSymbol" _) -> gStartOptionSymbol t
    (Rule "StartRepeatSymbol" _) -> gStartRepeatSymbol t
    (Rule "EndGroupSymbol" _)    -> gEndGroupSymbol t
    (Rule "EndOptionSymbol" _)   -> gEndOptionSymbol t
    (Rule "EndRepeatSymbol" _)   -> gEndRepeatSymbol t
    (Rule "DoubleQuoteSymbol" _) -> gDoubleQuoteSymbol t
    (Rule "RepetitionSymbol" _)  -> gRepetitionSymbol t
    (Rule "TerminatorSymbol" _)  -> gTerminatorSymbol t
    (Rule "OtherCharacter" _)    -> gOtherCharacter t

gSecondTerminalCharacter (Rule "SecondTerminalCharacter" t) = case t of
    (Rule "Letter" _)            -> gLetter t
    (Rule "DecimalDigit" _)      -> gDecimalDigit t
    (Rule "ConcatenateSymbol" _) -> gConcatenateSymbol t
    (Rule "AlternativeSymbol" _) -> gAlternativeSymbol t
    (Rule "DefiningSymbol" _)    -> gDefiningSymbol t
    (Rule "StartGroupSymbol" _)  -> gStartGroupSymbol t
    (Rule "StartOptionSymbol" _) -> gStartOptionSymbol t
    (Rule "StartRepeatSymbol" _) -> gStartRepeatSymbol t
    (Rule "EndGroupSymbol" _)    -> gEndGroupSymbol t
    (Rule "EndOptionSymbol" _)   -> gEndOptionSymbol t
    (Rule "EndRepeatSymbol" _)   -> gEndRepeatSymbol t
    (Rule "SingleQuoteSymbol" _) -> gSingleQuoteSymbol t
    (Rule "RepetitionSymbol" _)  -> gRepetitionSymbol t
    (Rule "TerminatorSymbol" _)  -> gTerminatorSymbol t
    (Rule "OtherCharacter" _)    -> gOtherCharacter t

gMetaIdentifier (Rule "MetaIdentifier" t) = case t of
    (Seq l r) -> gLetter l ++ gStar gMetaIdentifierCharacter r

gMetaIdentifierCharacter (Rule "MetaIdentifierCharacter" t) = case t of
    (Rule "Letter" _) -> gLetter t
    _                 -> gDecimalDigit t

gInteger (Rule "Integer" t) = case t of
    (Seq l r) -> gDecimalDigit l ++ gStar gDecimalDigit r
