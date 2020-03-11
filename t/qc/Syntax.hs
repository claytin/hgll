-- TODO
-- 1. Can some identation be implemented?
module Parser.Generator.Syntax ( gSyntax ) where

import Parser.Generator.Exception
import Control.Exception.Base

import Parser.Data.ParseTree

import Parser.Generator.Util
import Parser.Generator.Words
import Parser.Generator.CharSet


gSyntax (Rule "Syntax" t) = case t of
    (Seq l r) -> gSyntaxRule l ++ gClosure gSyntaxRule r
    _         -> throw GenFailure
gSyntax _ = throw GenFailure

gSyntaxRule (Rule "SyntaxRule" t) = case t of
    (Seq (Seq (Seq a b) c) d) ->
        gMetaIdentifier a ++ gSepS    ++ gDefiningSymbol b  ++ gSepS
                          ++ gQuoteS  ++ gMetaIdentifier a  ++ gQuoteS
                          ++ gRuleC b ++ gDefinitionsList c ++ gTerminatorS d
    _ -> throw GenFailure
gSyntaxRule _ = throw GenFailure

gDefinitionsList (Rule "DefinitionsList" t) = case t of
    (Seq l r) -> gSingleDefinition l ++ gClosure list r
        where
            list (Seq c d) = gAltC c ++ gSingleDefinition d
            list _         = throw GenFailure
    _         -> throw GenFailure
gDefinitionsList _ = throw GenFailure

gSingleDefinition (Rule "SingleDefinition" t) = case t of
    (Seq l r) -> gSyntacticTerm l ++ gClosure def r
        where
            def (Seq c t') = gSqncC c ++ gSyntacticTerm t'
            def _          = throw GenFailure
    _         -> throw GenFailure
gSingleDefinition _ = throw GenFailure

gSyntacticTerm (Rule "SyntacticTerm" t) = case t of
    (Seq l r) -> gSyntacticFactor l ++ gOptional except r
        where
            except (Seq c f) = gExceptC c ++ gSyntacticFactor f
            except _         = throw GenFailure
    _         -> throw GenFailure
gSyntacticTerm _ = throw GenFailure

gSyntacticFactor (Rule "SyntacticFactor" t) = case t of
    (Seq l r) -> gOptional nTimes l ++ gSyntacticPrimary r
        where
            nTimes (Seq n c) = gInteger n ++ gRepC c
            nTimes _         = throw GenFailure
    _         -> throw GenFailure
gSyntacticFactor _ = throw GenFailure

gSyntacticPrimary (Rule "SyntacticPrimary" t) = case t of
     (Rule "OptionalSequence" _) -> gOptionalSequence t
     (Rule "RepeatedSequence"_)  -> gRepeatedSequence t
     (Rule "GroupedSequence" _)  -> gGroupedSequence t
     (Rule "EmptySequence" _)    -> gEmptySequence t
     (Rule "MetaIdentifier" _)   -> gMetaIdentifier t
     (Rule "TerminalString" _)   -> gTerminalString t
     _                           -> throw GenFailure
gSyntacticPrimary _ = throw GenFailure

gOptionalSequence (Rule "OptionalSequence" t) = case t of
    (Seq (Seq _ l) _) ->
        gOptC ++ gOpenParnS ++ gDefinitionsList l ++ gCloseParnS
    _                 -> throw GenFailure
gOptionalSequence _ = throw GenFailure

gRepeatedSequence (Rule "RepeatedSequence" t) = case t of
    (Seq (Seq _ l) _) ->
        gClosureC ++ gOpenParnS ++ gDefinitionsList l ++ gCloseParnS
    _                 -> throw GenFailure
gRepeatedSequence _ = throw GenFailure

gGroupedSequence (Rule "GroupedSequence" t) = case t of
    (Seq (Seq _ l) _) ->
        gOpenParnS ++ gDefinitionsList l ++ gCloseParnS
    _                 -> throw GenFailure
gGroupedSequence _ = throw GenFailure

gEmptySequence (Rule "EmptySequence" t) = gEpsC
gEmptySequence _                        = throw GenFailure
