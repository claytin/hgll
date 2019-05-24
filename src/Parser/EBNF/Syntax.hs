module Parser.EBNF.Syntax ( syntax ) where

import Parser.Combinators.Ext
import Parser.EBNF.CharSet
import Parser.EBNF.Words

syntax     = rule "Syntax" [ syntaxRule +> star syntaxRule ]
syntaxRule = rule "SyntaxRule"
           [ metaIdentifier +> definingSymbol
                            +> definitionsList
                            +> terminatorSymbol ]

definitionsList = rule "DefinitionsList"
    [ singleDefinition +> star (alternativeSymbol +> singleDefinition) ]

-- In the definition of the rule SingleDefinition syntacticTerm should replace
-- syntacticFactor, but the first was removed because of "bad" redundance
singleDefinition = rule "SingleDefinition"
    [ syntacticFactor +> star (concatenateSymbol +> syntacticFactor) ]

syntacticFactor = rule "SyntacticFactor"
                [ opt (integer +> repetitionSymbol) +> syntacticPrimary ]

-- Note: the non-terminal emptySequence must come before metaIdentifier, since
-- the first is a meta identifier, but a special one, and it must be recognized
-- as such
syntacticPrimary = rule "SyntacticPrimary"
                 [ optionalSequence
                 , repeatedSequence
                 , groupedSequence
                 , emptySequence
                 , metaIdentifier
                 , terminalString ]

optionalSequence = rule "OptionalSequence"
                 [ startOptionSymbol +> definitionsList +> endOptionSymbol ]

repeatedSequence = rule "RepeatedSequence"
                 [ startRepeatSymbol +> definitionsList +> endRepeatSymbol ]

groupedSequence = rule "GroupedSequence"
                 [ startGroupSymbol +> definitionsList +> endGroupSymbol ]

-- This does not correspond to the ISO EBNF standard, but it is necessary
-- because it is "not possible" to determin if there is an empty production,
-- otherwise
emptySequence = rule "EmptySequence" [ t "eps" ]
