-- Only the rules needed by the EBNF meta sytnax (Parser.EBNF.Syntax) are
-- exported
module Parser.EBNF.Words ( terminalString
                         , integer
                         , metaIdentifier ) where

import Parser.Instance.K
import Parser.Combinators.ExtK

import Parser.EBNF.CharSet

terminalCharacter = "TerminalCharacter" =!>
                     letter
                 <|> decimalDigit
                 <|> concatenateSymbol
                 <|> definingSymbol
                 <|> alternativeSymbol     -- definition separator symbol
                 <|> endCommentSymbol
                 <|> endGroupSymbol
                 <|> endOptionSymbol
                 <|> endRepeatSymbol
                 <|> exceptSymbol
                 <|> firstQuoteSymbol
                 <|> repetitionSymbol
                 <|> secondQuoteSymbol
                 <|> specialSequenceSymbol
                 <|> startCommentSymbol
                 <|> startGroupSymbol
                 <|> startOptionSymbol
                 <|> startRepeatSymbol
                 <|> terminatorSymbol
                 <|> otherCharacter

terminalString = "TerminalString" =!>
                  firstQuoteSymbol # firstTerminalCharacter
                                   # star firstTerminalCharacter
                                   # firstQuoteSymbol
              <|> secondQuoteSymbol # secondTerminalCharacter
                                    # star secondTerminalCharacter
                                    # secondQuoteSymbol

firstTerminalCharacter = "FirstTerminalCharacter" =!>
    terminalCharacter -. firstQuoteSymbol

secondTerminalCharacter = "SecondTerminalCharacter" =!>
    terminalCharacter -. secondQuoteSymbol

integer = "Integer" =!> decimalDigit # star decimalDigit

metaIdentifier = "MetaIdentifier" =!> letter # star metaIdentifierCharacter

metaIdentifierCharacter = "MetaIdentifierCharacter" =!>
                           letter
                       <|> decimalDigit
