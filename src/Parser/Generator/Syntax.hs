-- TODO
-- 1. Can some identation be implemented?
module Parser.Generator.Syntax ( gSyntax ) where

import Parser.Data.ParseTree

import Parser.Generator.Util
import Parser.Generator.Words
import Parser.Generator.CharSet


gSyntax (Rule "Syntax" t) = case t of
    (Seq l r) -> gSyntaxRule l ++ gClosure gSyntaxRule r

gSyntaxRule (Rule "SyntaxRule" t) = case t of
    (Seq (Seq (Seq a b) c) d) ->
        gMetaIdentifier a ++ gSepS    ++ gDefiningSymbol b  ++ gSepS
                          ++ gQuoteS  ++ gMetaIdentifier a  ++ gQuoteS
                          ++ gRuleC b ++ gDefinitionsList c ++ gTerminatorS d

gDefinitionsList (Rule "DefinitionsList" t) = case t of
    (Seq l r) -> gSingleDefinition l ++ gClosure list r
        where
            list (Seq c d) = gAltC c ++ gSingleDefinition d

gSingleDefinition (Rule "SingleDefinition" t) = case t of
    (Seq l r) -> gSyntacticTerm l ++ gClosure def r
        where
            def (Seq c t') = gSqncC c ++ gSyntacticTerm t'

gSyntacticTerm (Rule "SyntacticTerm" t) = case t of
    (Seq l r) -> gSyntacticFactor l ++ gOptional except r
        where
            except (Seq c f) = gExceptC c ++ gSyntacticFactor f

gSyntacticFactor (Rule "SyntacticFactor" t) = case t of
    (Seq l r) -> gOptional nTimes l ++ gSyntacticPrimary r
        where
            nTimes (Seq n c) = gInteger n ++ gRepC c

gSyntacticPrimary (Rule "SyntacticPrimary" t) = case t of
     (Rule "OptionalSequence" _) -> gOptionalSequence t
     (Rule "RepeatedSequence"_)  -> gRepeatedSequence t
     (Rule "GroupedSequence" _)  -> gGroupedSequence t
     (Rule "EmptySequence" _)    -> gEmptySequence t
     (Rule "MetaIdentifier" _)   -> gMetaIdentifier t
     (Rule "TerminalString" _)   -> gTerminalString t

gOptionalSequence (Rule "OptionalSequence" t) = case t of
    (Seq (Seq _ l) _) ->
        gOptC ++ gOpenParnS ++ gDefinitionsList l ++ gCloseParnS

gRepeatedSequence (Rule "RepeatedSequence" t) = case t of
    (Seq (Seq _ l) _) ->
        gClosureC ++ gOpenParnS ++ gDefinitionsList l ++ gCloseParnS

gGroupedSequence (Rule "GroupedSequence" t) = case t of
    (Seq (Seq _ l) _) ->
        gOpenParnS ++ gDefinitionsList l ++ gCloseParnS

gEmptySequence (Rule "EmptySequence" t) = gEpsC
