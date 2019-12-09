module Parser.EBNF.Syntax ( syntax ) where

import Parser.Instance.K
import Parser.Combinators.ExtK

import Parser.EBNF.CharSet
import Parser.EBNF.Words

syntax     = "Syntax"     =!> syntaxRule # star syntaxRule
syntaxRule = "SyntaxRule" =!>
    metaIdentifier # definingSymbol # definitionsList # terminatorSymbol

definitionsList = "DefinitionsList" =!>
    singleDefinition # star (alternativeSymbol # singleDefinition)

singleDefinition = "SingleDefinition" =!>
     syntacticTerm # star (concatenateSymbol # syntacticTerm)

syntacticTerm = "SyntacticTerm" =!>
    syntacticFactor # opt (exceptSymbol # syntacticFactor)

syntacticFactor = "SyntacticFactor" =!>
     opt (integer # repetitionSymbol) # syntacticPrimary

syntacticPrimary = "SyntacticPrimary" =!>
                    optionalSequence
                <|> repeatedSequence
                <|> groupedSequence
                <|> metaIdentifier
                <|> terminalString
                <|> emptySequence

optionalSequence = "OptionalSequence" =!>
    startOptionSymbol # definitionsList # endOptionSymbol

repeatedSequence = "RepeatedSequence" =!>
    startRepeatSymbol # definitionsList # endRepeatSymbol

groupedSequence = "GroupedSequence" =!>
    startGroupSymbol # definitionsList # endGroupSymbol

emptySequence = "EmptySequence" =!> eps
