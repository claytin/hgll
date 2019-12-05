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

-- In the definition of the rule SingleDefinition syntacticTerm should replace
-- syntacticFactor, but the first was removed because rules based on set
-- diferences, described by syntacticTerm, are not supported
singleDefinition = "SingleDefinition" =!>
     syntacticFactor # star (concatenateSymbol # syntacticFactor)

syntacticFactor = "SyntacticFactor" =!>
     opt (integer # repetitionSymbol) # syntacticPrimary

-- Note: the non-terminal emptySequence must come before metaIdentifier, since
-- the first is a meta identifier, but a special one, and it must be recognized
-- as such
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

-- This does not correspond to the ISO EBNF standard, but it is necessary
-- because it is "not possible" to distinguish an empty production otherwise
-- emptySequence = "EmptySequence" =!> t "eps"
emptySequence = "EmptySequence" =!> eps
