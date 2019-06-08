-- Only the rules needed by the EBNF meta sytnax (Parser.EBNF.Syntax) are
-- exported
module Parser.EBNF.Words ( terminalString
                         , metaIdentifier
                         , integer ) where

import Parser.Instance.K
import Parser.Combinators.ExtK

import Parser.EBNF.CharSet

terminalString = "TerminalString" =!>
                  singleQuoteSymbol # firstTerminalCharacter
                                    # star firstTerminalCharacter
                                    # singleQuoteSymbol
              <|> doubleQuoteSymbol # secondTerminalCharacter
                                    # star secondTerminalCharacter
                                    # doubleQuoteSymbol

firstTerminalCharacter = "FirstTerminalCharacter" =!>
                          letter
                      <|> decimalDigit
                      <|> concatenateSymbol
                      <|> alternativeSymbol
                      <|> definingSymbol
                      <|> startGroupSymbol
                      <|> startOptionSymbol
                      <|> startRepeatSymbol
                      <|> endGroupSymbol
                      <|> endOptionSymbol
                      <|> endRepeatSymbol
                      <|> doubleQuoteSymbol
                      <|> repetitionSymbol
                      <|> terminatorSymbol
                      <|> otherCharacter

secondTerminalCharacter = "SecondTerminalCharacter" =!>
                           letter
                       <|> decimalDigit
                       <|> concatenateSymbol
                       <|> alternativeSymbol
                       <|> definingSymbol
                       <|> startGroupSymbol
                       <|> startOptionSymbol
                       <|> startRepeatSymbol
                       <|> endGroupSymbol
                       <|> endOptionSymbol
                       <|> endRepeatSymbol
                       <|> singleQuoteSymbol
                       <|> repetitionSymbol
                       <|> terminatorSymbol
                       <|> otherCharacter

metaIdentifier = "MetaIdentifier" =!> letter # star metaIdentifierCharacter

metaIdentifierCharacter = "MetaIdentifierCharacter" =!>
                           letter
                       <|> decimalDigit

integer = "Integer" =!> decimalDigit # star decimalDigit
