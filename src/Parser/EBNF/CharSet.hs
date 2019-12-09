-- This module defines the ISO/IEC 646:1991 (according to the ISO/IEC 14977
-- EBNF standard) 7 bit character set.
--
module Parser.EBNF.CharSet ( letter
                           , decimalDigit
                           , concatenateSymbol
                           , definingSymbol
                           , alternativeSymbol
                           , endCommentSymbol
                           , endGroupSymbol
                           , endOptionSymbol
                           , endRepeatSymbol
                           , exceptSymbol
                           , firstQuoteSymbol
                           , repetitionSymbol
                           , secondQuoteSymbol
                           , specialSequenceSymbol
                           , startCommentSymbol
                           , startGroupSymbol
                           , startOptionSymbol
                           , startRepeatSymbol
                           , terminatorSymbol
                           , otherCharacter ) where

import Parser.Instance.K
import Parser.Combinators.ExtK

letter = "Letter" =!>
          t "a" <|> t "b" <|> t "c" <|> t "d" <|> t "e" <|> t "f" <|> t "g"
      <|> t "h" <|> t "i" <|> t "j" <|> t "k" <|> t "l" <|> t "m" <|> t "n"
      <|> t "o" <|> t "p" <|> t "q" <|> t "r" <|> t "s" <|> t "t" <|> t "u"
      <|> t "v" <|> t "w" <|> t "x" <|> t "y" <|> t "z"
      <|> t "A" <|> t "B" <|> t "C" <|> t "D" <|> t "E" <|> t "F" <|> t "G"
      <|> t "H" <|> t "I" <|> t "J" <|> t "K" <|> t "L" <|> t "M" <|> t "N"
      <|> t "O" <|> t "P" <|> t "Q" <|> t "R" <|> t "S" <|> t "T" <|> t "U"
      <|> t "V" <|> t "W" <|> t "X" <|> t "Y" <|> t "Z"

decimalDigit = "DecimalDigit" =!>
                t "0" <|> t "1" <|> t "2" <|> t "3" <|> t "4"
            <|> t "5" <|> t "6" <|> t "7" <|> t "8" <|> t "9"

concatenateSymbol     = "ConcatenateSymbol"     =!> t ","
definingSymbol        = "DefiningSymbol"        =!> t "="
alternativeSymbol     = "AlternativeSymbol"     =!> t "|" <|> t "/" <|> t "!"
endCommentSymbol      = "EndCommentSymbol"      =!> t "*)"
endGroupSymbol        = "EndGroupSymbol"        =!> t ")"
endOptionSymbol       = "EndOptionSymbol"       =!> t "]" <|> t "/)"
endRepeatSymbol       = "EndRepeatSymbol"       =!> t "}" <|> t ":)"
exceptSymbol          = "ExceptSymbol"          =!> t "-"
firstQuoteSymbol      = "FirstQuoteSymbol"      =!> t "'"
repetitionSymbol      = "RepetitionSymbol"      =!> t "*"
secondQuoteSymbol     = "SecondQuoteSymbol"     =!> t "\""
specialSequenceSymbol = "SpecialSequenceSymbol" =!> t "?"
startCommentSymbol    = "StartCommentSymbol"    =!> t "(*"
startGroupSymbol      = "StartGroupSymbol"      =!> t "("
startOptionSymbol     = "StartOptionSymbol"     =!> t "[" <|> t "(/"
startRepeatSymbol     = "StartRepeatSymbol"     =!> t "{" <|> t "(:"
terminatorSymbol      = "TerminatorSymbol"      =!> t ";" <|> t "."

otherCharacter = "OtherCharacter" =!>
                  spaceCharacter
              <|> t ":" <|> t "+" <|> t "_" <|> t "%" <|> t "@"
              <|> t "&" <|> t "#" <|> t "$" <|> t "<" <|> t ">"
              <|> t "^" <|> t "`" <|> t "~"
              <|> t "\\"

-- The parsers bellow are not exported, at least for now. The reason being,
-- gaps are not used by the EBNF meta syntax, and are trimmed from the input
spaceCharacter = "SpaceCharacter" =!> t " "

newLine = "NewLine" =!> star carriageReturn # t "\n" # star carriageReturn

horizontalTabulationCharacter = "HorizontalTabulationCharacter" =!> t "\t"
verticalTabulationCharacter   = "VerticalTabulationCharacter"   =!> t "\v"

formFeed       = "FormFeed"       =!> t "\f"
carriageReturn = "CarriageReturn" =!> t "\r"
