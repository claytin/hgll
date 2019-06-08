-- TODO
-- 1. Add spaces for readability, it may also help Haskell not to get confused
-- 2. Can some identation be implemented?
module Parser.Generator.Syntax ( gSyntax ) where

import Parser.Data.ParseTree
import Parser.Generator.Util (gTerm, gOpt, gStar)

import Parser.Generator.Words
import Parser.Generator.CharSet


gSyntax (Rule "Syntax" t) = case t of
    (Seq l r) -> gSyntaxRule l ++ gStar gSyntaxRule r

gSyntaxRule (Rule "SyntaxRule" t) = case t of
    (Seq (Seq (Seq l m) n) r) -> gMetaIdentifier l
                              ++ gDefiningSymbol m
                              ++ " \"" ++ gMetaIdentifier l ++ "\" =|> "
                              ++ gDefinitionsList n
                              ++ gTerminatorSymbol r ++ "\n"

gDefinitionsList (Rule "DefinitionsList" t) = case t of
    (Seq l r) -> gSingleDefinition l ++ gStar list r
        where
            list (Seq s d) = gAlternativeSymbol s ++ gSingleDefinition d

gSingleDefinition (Rule "SingleDefinition" t) = case t of
    (Seq l r) -> gSyntacticFactor l ++ gStar def r
        where
            def (Seq s f) = gConcatenateSymbol s ++ gSyntacticFactor f

gSyntacticFactor (Rule "SyntacticFactor" t) = case t of
    (Seq l r) -> gOpt nTimes l ++ gSyntacticPrimary r
        where
            nTimes (Seq n s) = gInteger n ++ gRepetitionSymbol s

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
                      ++ "\"OptionalSequence\" =|> "
                      ++ gDefinitionsList m
                      ++ gEndOptionSymbol r

gRepeatedSequence (Rule "RepeatedSequence" t) = case t of
    (Seq (Seq l m) r) -> "star "
                      ++ gStartRepeatSymbol l
                      ++ "\"RepeatedSequence\" =|> "
                      ++ gDefinitionsList m
                      ++ gEndRepeatSymbol r

gGroupedSequence (Rule "GroupedSequence" t) = case t of
    (Seq (Seq l m) r) -> gStartGroupSymbol l
                      ++ "\"GroupedSequence\" =|> "
                      ++ gDefinitionsList m
                      ++ gEndGroupSymbol r

gEmptySequence (Rule "EmptySequence" t) = gTerm t
