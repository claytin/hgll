module Parser.EBNF.Words ( terminalString
                         , metaIdentifier
                         , integer ) where

import Parser.Combinators.Ext
import Parser.EBNF.CharSet

terminalString = rule "TerminalString"
               [ singleQuoteSymbol +> firstTerminalCharacter
                                   +> star firstTerminalCharacter
                                   +> singleQuoteSymbol
               , doubleQuoteSymbol +> secondTerminalCharacter
                                   +> star secondTerminalCharacter
                                   +> doubleQuoteSymbol ]

firstTerminalCharacter = rule "FirstTerminalCharacter"
                       [ letter
                       , decimalDigit
                       , concatenateSymbol
                       , alternativeSymbol
                       , definingSymbol
                       , startGroupSymbol
                       , startOptionSymbol
                       , startRepeatSymbol
                       , endGroupSymbol
                       , endOptionSymbol
                       , endRepeatSymbol
                       , doubleQuoteSymbol
                       , repetitionSymbol
                       , terminatorSymbol
                       , otherCharacter ]

secondTerminalCharacter = rule "SecondTerminalCharacter"
                        [ letter
                        , decimalDigit
                        , concatenateSymbol
                        , alternativeSymbol
                        , definingSymbol
                        , startGroupSymbol
                        , startOptionSymbol
                        , startRepeatSymbol
                        , endGroupSymbol
                        , endOptionSymbol
                        , endRepeatSymbol
                        , singleQuoteSymbol
                        , repetitionSymbol
                        , terminatorSymbol
                        , otherCharacter ]

metaIdentifier = rule "MetaIdentifier"
               [ letter +> star metaIdentifierCharacter ]

metaIdentifierCharacter = rule "MetaIdentifierCharacter"
                        [ letter
                        , decimalDigit ]

integer = rule "Integer" [ decimalDigit +> star decimalDigit ]
