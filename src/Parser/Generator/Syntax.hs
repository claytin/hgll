-- TODO
-- 1. Add spaces for readability, it may also help Haskell not to get confused
-- 2. Can some identation be implemented?
module Parser.Generator.Syntax ( gSyntax ) where

import Parser.Data.ParseTree
import Parser.Generator.Util

import Parser.Generator.Words
import Parser.Generator.CharSet


gSyntax (Rule "Syntax" t) = case t of
    (Seq l r) -> gSyntaxRule l ++ gStar gSyntaxRule r

gSyntaxRule (Rule "SyntaxRule" t) = case t of
    (Seq (Seq (Seq l m) n) _) -> gMetaIdentifier l
                              ++ " " ++ gDefiningSymbol m
                              ++ " \"" ++ gMetaIdentifier l ++ "\""
                              ++ cRuleDefOp
                              ++ gDefinitionsList n
                              ++ cRuleTermSym

gDefinitionsList (Rule "DefinitionsList" t) = case t of
    (Seq l r) -> gSingleDefinition l ++ gStar list r
        where
            list (Seq _ d) = cAltOp ++ gSingleDefinition d

gSingleDefinition (Rule "SingleDefinition" t) = case t of
    (Seq l r) -> gSyntacticFactor l ++ gStar def r
        where
            def (Seq _ f) = cSeqOp ++ gSyntacticFactor f

gSyntacticFactor (Rule "SyntacticFactor" t) = case t of
    (Seq l r) -> gOpt nTimes l ++ gSyntacticPrimary r
        where
            nTimes (Seq n _) = gInteger n ++ ecRepOp

gSyntacticPrimary (Rule "SyntacticPrimary" t) = case t of
     (Rule "OptionalSequence" _) -> gOptionalSequence t
     (Rule "RepeatedSequence"_)  -> gRepeatedSequence t
     (Rule "GroupedSequence" _)  -> gGroupedSequence t
     (Rule "EmptySequence" _)    -> gEmptySequence t
     (Rule "MetaIdentifier" _)   -> gMetaIdentifier t
     (Rule "TerminalString" _)   -> gTerminalString t

gOptionalSequence (Rule "OptionalSequence" t) = case t of
    (Seq (Seq _ l) _) ->
        ecOptOp ++ cOSSeqSym ++ gDefinitionsList l ++ cCSSeqSym

gRepeatedSequence (Rule "RepeatedSequence" t) = case t of
    (Seq (Seq _ l) _) ->
        ecClosureOp ++ cOSSeqSym ++ gDefinitionsList l ++ cCSSeqSym

gGroupedSequence (Rule "GroupedSequence" t) = case t of
    (Seq (Seq _ l) _) ->
        cOSSeqSym ++ gDefinitionsList l ++ cCSSeqSym

gEmptySequence (Rule "EmptySequence" t) = gTerm t
