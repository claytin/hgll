module Parser.EBNF.Words ( terminalString
                         , metaIdentifier
                         , integer ) where

import Parser.Combinators.Base
import Parser.EBNF.CharSet

terminalString = rule "TerminalString"
               [ singleQuoteSymbol +> firstTerminalCharacterMany
                                   +> singleQuoteSymbol
               , doubleQuoteSymbol +> secondTerminalCharacterMany
                                   +> doubleQuoteSymbol ]

firstTerminalCharacterMany = rule "FirstTerminalCharacterMany"
    [ firstTerminalCharacter +> firstTerminalCharacterMany
    , firstTerminalCharacter ]

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

secondTerminalCharacterMany = rule "SecondTerminalCharacterMany"
    [ secondTerminalCharacter +> secondTerminalCharacterMany
    , secondTerminalCharacter ]

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
               [ letter +> metaIdentifierCharacterMany
               , letter ]

metaIdentifierCharacterMany = rule "MetaIdentifierCharacterMany"
    [ metaIdentifierCharacter +> metaIdentifierCharacterMany
    , metaIdentifierCharacter ]

metaIdentifierCharacter = rule "MetaIdentifierCharacter"
                        [ letter
                        , decimalDigit ]

-- Integer and DecimalDigitMany are redundant, but i like this redundance
integer          = rule "Integer" [ decimalDigitMany ]
decimalDigitMany = rule "DecimalDigitMany"
                 [ decimalDigit +> decimalDigitMany
                 , decimalDigit ]
