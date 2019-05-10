-- TODO
-- 1. Add spaces for readability, it may also help Haskell not getting
-- confused (?)

module Parser.Generator.Syntax ( gSyntax ) where

import Parser.Data.ParseTree
import Parser.Generator.Words
import Parser.Generator.CharSet

-- These imports are for testing only (cause f u ghci), they must be commented
import Parser.Combinators.Base
import Parser.EBNF.Syntax


gSyntax (Rule "Syntax" t) = gSyntaxRuleMany t

-- The first equation unpacks the top level rule layer and it is necessary
-- because of the recursive repetition pattern describe by the rule. This
-- rationale apllies for all the *Many functions
gSyntaxRuleMany t = case t of
    (Rule "SyntaxRuleMany" t') ->
        gSyntaxRuleMany t'
    (Seq l r) ->
        gSyntaxRule l ++ gSyntaxRuleMany r
    _ ->
        gSyntaxRule t

gSyntaxRule (Rule "SyntaxRule" t) = case t of
    (Seq (Seq (Seq l m) n) r) -> gMetaIdentifier l
                              ++ gDefiningSymbol m
                              ++ "rule \"" ++ gMetaIdentifier l ++ "\" "
                              ++ "[" ++ gDefinitionList n ++ "]"
                              ++ gTerminatorSymbol r ++ "\n"

gDefinitionList (Rule "DefinitionList" t) = case t of
    (Seq l r) -> gSingleDefinition l ++ gDefinitionListMany r
    _         -> gSingleDefinition t

gDefinitionListMany t = case t of
    (Rule "DefinitionListMany" t') ->
        gDefinitionListMany t'
    (Seq (Seq l m) r) ->
        gAlternativeSymbol l ++ gSingleDefinition m ++ gDefinitionListMany r
    (Seq l r) ->
        gAlternativeSymbol l ++ gSingleDefinition r

gSingleDefinition (Rule "SingleDefinition" t) = case t of
    (Seq l r) -> gSyntacticFactor l ++ gSingleDefinitionMany r
    _         -> gSyntacticFactor t

gSingleDefinitionMany t = case t of
    (Rule "SingleDefinitionMany" t') ->
        gSingleDefinitionMany t'
    (Seq (Seq l m) r) ->
        gConcatenateSymbol l ++ gSyntacticFactor m ++ gSingleDefinitionMany r
    (Seq l r) ->
        gConcatenateSymbol l ++ gSyntacticFactor r

gSyntacticFactor (Rule "SyntacticFactor" t) = case t of
    (Seq (Seq l m) r) ->
        join " +> " $ n $ repeat (gSyntacticPrimary r)
        where
            join s = foldl1 (\l r -> l ++ s ++ r)
            n      = take (read $ gInteger l)
    _ ->
        gSyntacticPrimary t

gSyntacticPrimary (Rule "SyntacticPrimary" t) = case t of
     (Rule "OptionalSequence" _) -> gOptionalSequence t
     (Rule "RepeatedSequence"_)  -> gRepeatedSequence t
     (Rule "GroupedSequence" _)  -> gGroupedSequence t
     (Rule "EmptySequence" _)    -> gEmptySequence t
     (Rule "MetaIdentifier" _)   -> gMetaIdentifier t
     (Rule "TerminalString" _)   -> gTerminalString t

gOptionalSequence (Rule "OptionalSequence" t) = case t of
    (Seq (Seq l m) r) -> "opt "
                      ++ gStartOptionSymbol l
                      ++ "rule \"OptionalSequence\" "
                      ++ "[" ++ gDefinitionList m ++ "]"
                      ++ gEndOptionSymbol r

gRepeatedSequence (Rule "RepeatedSequence" t) = case t of
    (Seq (Seq l m) r) -> "star "
                      ++ gStartRepeatSymbol l
                      ++ "rule \"RepeatedSequence\" "
                      ++ "[" ++ gDefinitionList m ++ "]"
                      ++ gEndRepeatSymbol r

gGroupedSequence (Rule "GroupedSequence" t) = case t of
    (Seq (Seq l m) r) -> gStartGroupSymbol l
                      ++ "rule \"GroupedSequence\" "
                      ++ "[" ++ gDefinitionList m ++ "]"
                      ++ gEndGroupSymbol r

gEmptySequence (Rule "EmptySequence" (Token tk)) = tk
