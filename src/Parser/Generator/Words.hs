module Parser.Generator.Words ( gTerminalString
                              , gInteger
                              , gMetaIdentifier ) where

import Parser.Data.ParseTree

import Parser.Generator.Util
import Parser.Generator.CharSet

gTerminalCharacter (Rule "TerminalCharacter" t) = case t of
    (Rule "Letter" _)                -> gLetter t
    (Rule "DecimalDigit" _)          -> gDecimalDigit t
    (Rule "ConcatenateSymbol" _)     -> gConcatenateSymbol t
    (Rule "DefiningSymbol" _)        -> gDefiningSymbol t
    (Rule "AlternativeSymbol" _)     -> gAlternativeSymbol t
    (Rule "EndCommentSymbol"_)       -> gEndCommentSymbol t
    (Rule "EndGroupSymbol" _)        -> gEndGroupSymbol t
    (Rule "EndOptionSymbol" _)       -> gEndOptionSymbol t
    (Rule "EndRepeatSymbol" _)       -> gEndRepeatSymbol t
    (Rule "ExceptSymbol" _)          -> gExceptSymbol t
    (Rule "FirstQuoteSymbol" _)      -> gFirstQuoteSymbol t
    (Rule "RepetitionSymbol" _)      -> gRepetitionSymbol t
    (Rule "SecondQuoteSymbol" _)     -> gSecondQuoteSymbol t
    (Rule "SpecialSequenceSymbol" _) -> gSpecialSequenceSymbol t
    (Rule "StartCommentSymbol"_)     -> gStartCommentSymbol t
    (Rule "StartGroupSymbol" _)      -> gStartGroupSymbol t
    (Rule "StartOptionSymbol" _)     -> gStartOptionSymbol t
    (Rule "StartRepeatSymbol" _)     -> gStartRepeatSymbol t
    (Rule "TerminatorSymbol" _)      -> gTerminatorSymbol t
    (Rule "OtherCharacter" _)        -> gOtherCharacter t

gTerminalString (Rule "TerminalString" t) = case t of
    (Seq (Seq (Seq a@(Rule "FirstQuoteSymbol" _) b) c) d) ->
        gTermC ++ gFirstQuoteS a
               ++ gFirstTerminalCharacter b
               ++ gClosure gFirstTerminalCharacter c
               ++ gFirstQuoteS d
    (Seq (Seq (Seq a@(Rule "SecondQuoteSymbol" _) b) c) d) ->
        gTermC ++ gSecondQuoteS a
               ++ gSecondTerminalCharacter b
               ++ gClosure gSecondTerminalCharacter c
               ++ gSecondQuoteS d

-- *TerminalCharacter are just TerminalCharacter, once established, by the
-- pattern matching in the g*TerminalCharacter functions, which subset we are
-- dealing with, we only need to defer to gTerminalCharacter
gFirstTerminalCharacter (Rule "FirstTerminalCharacter" t) =
    gTerminalCharacter t

gSecondTerminalCharacter (Rule "SecondTerminalCharacter" t) =
    gTerminalCharacter t

gInteger (Rule "Integer" t) = case t of
    (Seq l r) -> gDecimalDigit l ++ gClosure gDecimalDigit r

gMetaIdentifier (Rule "MetaIdentifier" t) = case t of
    (Seq l r) -> gLetter l ++ gClosure gMetaIdentifierCharacter r

gMetaIdentifierCharacter (Rule "MetaIdentifierCharacter" t) = case t of
    (Rule "Letter" _) -> gLetter t
    _                 -> gDecimalDigit t
