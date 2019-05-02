module Parser.EBNF.Syntax ( syntax ) where

import Parser.Combinators.Base
import Parser.EBNF.CharSet
import Parser.EBNF.Words

syntax         = rule "Syntax" [ syntaxRuleMany ]
syntaxRuleMany = rule "SyntaxRuleMany"
               [ syntaxRule +> syntaxRuleMany
               , syntaxRule ]
syntaxRule     = rule "SyntaxRule"
               [ metaIdentifier +> definingSymbol +> definitionList
                                                  +> terminatorSymbol ]

definitionList     = rule "DefinitionList"
                   [ singleDefinition +> definitionListMany
                   , singleDefinition ]
definitionListMany = rule "DefinitionListMany"
                   [ alternativeSymbol +> singleDefinition
                                       +> definitionListMany
                   , alternativeSymbol +> singleDefinition ]

-- In the definition of the rule SingleDefinition syntacticTerm should replace
-- syntacticFactor, but we removed the first because of bad redundance
singleDefinition     = rule "SingleDefinition"
                     [ syntacticFactor +> singleDefinitionMany
                     , syntacticFactor ]
singleDefinitionMany = rule "SingleDefinitionMany"
                     [ concatenateSymbol +> syntacticFactor
                                         +> singleDefinitionMany
                     , concatenateSymbol +> syntacticFactor ]

syntacticFactor = rule "SyntacticFactor"
                [ integer +> repetitionSymbol +> syntacticPrimary
                , syntacticPrimary ]

-- Note: the non-terminal emptySequence must come before metaIdentifier, since
-- the first is a meta identifier, but a special one, and it must be identified
-- as such
syntacticPrimary = rule "SyntacticPrimary"
                 [ optionalSequence
                 , repeatedSequence
                 , groupedSequence
                 , emptySequence
                 , metaIdentifier
                 , terminalString ]

optionalSequence = rule "OptionalSequence"
                 [ startOptionSymbol +> definitionList +> endOptionSymbol ]

repeatedSequence = rule "RepeatedSequence"
                 [ startRepeatSymbol +> definitionList +> endRepeatSymbol ]

groupedSequence = rule "GroupedSequence"
                 [ startGroupSymbol +> definitionList +> endGroupSymbol ]

-- This does not correspond to the ISO EBNF standard
emptySequence = rule "EmptySequence" [ t "eps" ]
