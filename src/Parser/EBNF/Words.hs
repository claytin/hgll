-- Only the rules needed by the EBNF meta sytnax (Parser.EBNF.Syntax) are
-- exported
module Parser.EBNF.Words ( terminalString
                         , metaIdentifier
                         , integer ) where

import Parser.Instance.K
import Parser.Combinators.ExtK

import Parser.EBNF.CharSet

terminalString = "TerminalString" =!>
                  firstQuoteSymbol # firstTerminalCharacter
                                   # star firstTerminalCharacter
                                   # firstQuoteSymbol
              <|> secondQuoteSymbol # secondTerminalCharacter
                                    # star secondTerminalCharacter
                                    # secondQuoteSymbol

firstTerminalCharacter = "FirstTerminalCharacter" =!>
                          letter
                      <|> decimalDigit
                      <|> concatenateSymbol
                      <|> definingSymbol
                      <|> alternativeSymbol
                      <|> endGroupSymbol
                      <|> endOptionSymbol
                      <|> endRepeatSymbol
                      <|> exceptSymbol
                      <|> repetitionSymbol
                      <|> secondQuoteSymbol
                      <|> specialSequenceSymbol
                      <|> startGroupSymbol
                      <|> startOptionSymbol
                      <|> startRepeatSymbol
                      <|> terminatorSymbol
                      <|> otherCharacter

secondTerminalCharacter = "SecondTerminalCharacter" =!>
                           letter
                       <|> decimalDigit
                       <|> concatenateSymbol
                       <|> definingSymbol
                       <|> alternativeSymbol
                       <|> endGroupSymbol
                       <|> endOptionSymbol
                       <|> endRepeatSymbol
                       <|> exceptSymbol
                       <|> firstQuoteSymbol
                       <|> repetitionSymbol
                       <|> specialSequenceSymbol
                       <|> startGroupSymbol
                       <|> startOptionSymbol
                       <|> startRepeatSymbol
                       <|> terminatorSymbol
                       <|> otherCharacter

metaIdentifier = "MetaIdentifier" =!> letter # star metaIdentifierCharacter

metaIdentifierCharacter = "MetaIdentifierCharacter" =!>
                           letter
                       <|> decimalDigit

integer = "Integer" =!> decimalDigit # star decimalDigit
