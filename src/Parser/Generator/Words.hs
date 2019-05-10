-- Only the top level g functions are exported, they are the only ones needed,
-- and acctauly defined by the ISO standard
module Parser.Generator.Words ( gTerminalString
                              , gMetaIdentifier
                              , gInteger ) where

import Parser.Data.ParseTree
import Parser.Generator.CharSet

-- These imports are for testing only (cause f u ghci), they must be commented
import Parser.Combinators.Base
import Parser.EBNF.Words

gTerminalString (Rule "TerminalString" t) = case t of
    (Seq (Seq l@(Rule "SingleQuoteSymbol" _) m) r) ->
        "t " ++ gSingleQuoteSymbol l
             ++ gFirstTerminalCharacterMany m
             ++ gSingleQuoteSymbol r
    (Seq (Seq l@(Rule "DoubleQuoteSymbol" _) m) r) ->
        "t " ++ gDoubleQuoteSymbol l
             ++ gSecondTerminalCharacterMany m
             ++ gDoubleQuoteSymbol r

-- The first equation unpacks the top level rule layer and it is necessary
-- because of the recursive repetition pattern describe by the rule. This
-- rationale apllies for all the *Many functions
gFirstTerminalCharacterMany t = case t of
    (Rule "FirstTerminalCharacterMany" t') ->
        gFirstTerminalCharacterMany t'
    (Seq l r) ->
        gFirstTerminalCharacter l ++ gFirstTerminalCharacterMany r
    _ ->
        gFirstTerminalCharacter t

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

gSecondTerminalCharacterMany t = case t of
    (Rule "SecondTerminalCharacterMany" t') ->
        gSecondTerminalCharacterMany t'
    (Seq l r) ->
        gSecondTerminalCharacter l ++ gSecondTerminalCharacterMany r
    _ ->
        gSecondTerminalCharacter t

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

--
gMetaIdentifier (Rule "MetaIdentifier" t) = case t of
    (Seq l r) -> gLetter l ++ gMetaIdentifierCharacterMany r
    _         -> gLetter t

gMetaIdentifierCharacterMany t = case t of
    (Rule "MetaIdentifierCharacterMany" t') ->
        gMetaIdentifierCharacterMany t'
    (Seq l r) ->
        gMetaIdentifierCharacter l ++ gMetaIdentifierCharacterMany r
    _ ->
        gMetaIdentifierCharacter t

gMetaIdentifierCharacter (Rule "MetaIdentifierCharacter" t) = case t of
    (Rule "Letter" _) -> gLetter t
    _                 -> gDecimalDigit t

-- See Parser/EBNF/Words for info on this redundancy
gInteger (Rule _ t) = gDecimalDigitMany t

gDecimalDigitMany t = case t of
    (Rule "DecimalDigitMany" t) ->
        gDecimalDigitMany t
    (Seq l r) ->
        gDecimalDigit l ++ gDecimalDigitMany r
    _ ->
        gDecimalDigit t
