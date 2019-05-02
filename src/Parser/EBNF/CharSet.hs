module Parser.EBNF.CharSet ( letter
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
                           , doubleQuoteSymbol
                           , repetitionSymbol
                           , terminatorSymbol
                           , otherCharacter ) where

import Parser.Combinators.Base

-- This module defines the ISO/IEC 646:1991 (according to the ISO/IEC 14977
-- EBNF standard) 7 bit character set.
--
-- Note: this module does not define parsers for the special sequence symbol,
-- except symbol, start/end comment symbol. This rules are not supported (or in
-- any other way treated by this implementation, trying and use them will lead
-- to parsing errors

letter = rule "Letter"
       [ t "a" , t "b" , t "c" , t "d" , t "e" , t "f" , t "g" , t "h" , t "i"
       , t "j" , t "k" , t "l" , t "m" , t "n" , t "o" , t "p" , t "q" , t "r"
       , t "s" , t "t" , t "u" , t "v" , t "w" , t "x" , t "y" , t "z"
       , t "A" , t "B" , t "C" , t "D" , t "E" , t "F" , t "G" , t "H" , t "I"
       , t "J" , t "K" , t "L" , t "M" , t "N" , t "O" , t "P" , t "Q" , t "R"
       , t "S" , t "T" , t "U" , t "V" , t "W" , t "X" , t "Y" , t "Z" ]

decimalDigit = rule "DecimalDigit"
             [ t "0" , t "1" , t "2" , t "3" , t "4"
             , t "5" , t "6" , t "7" , t "8" , t "9" ]

concatenateSymbol = rule "ConcatenateSymbol" [ t "," ]
definingSymbol    = rule "DefiningSymbol"    [ t "=" ]
alternativeSymbol = rule "AlternativeSymbol" [ t "|" , t "/" , t "!" ]

startGroupSymbol  = rule "StartGroupSymbol"  [ t "(" ]
startOptionSymbol = rule "StartOptionSymbol" [ t "[" , t "(/" ]
startRepeatSymbol = rule "StartRepeatSymbol" [ t "{" , t "(:" ]

endGroupSymbol  = rule "EndGroupSymbol"  [ t ")" ]
endOptionSymbol = rule "EndOptionSymbol" [ t "]" , t "/)" ]
endRepeatSymbol = rule "EndRepeatSymbol" [ t "}" , t ":)" ]

singleQuoteSymbol = rule "SingleQuoteSymbol" [ t "'" ]
doubleQuoteSymbol = rule "DoubleQuoteSymbol" [ t "\"" ]

repetitionSymbol = rule "RepetitionSymbol" [ t "*" ]

terminatorSymbol = rule "TerminatorSymbol" [ t ";" , t "." ]

otherCharacter = rule "OtherCharacter"
               [ spaceCharacter , t ":" , t "+" , t "_" , t "%" , t "@"
               , t "&" , t "#" , t "$" , t "<" , t ">" , t "\\" , t "~"
               , t "`" , t "~" ]

-- The rules parsers bellow are not exported, at least for now, by this module.
-- The reason being that gaps will be trimmed from the input
spaceCharacter = rule "SpaceCharacter" [ t " " ]

-- Rules NewLine and CarriageReturnMany that are composed by more than just
-- terminals are longest match parsers
newLine = rule "NewLine"
        [ carriageReturnMany +> t "\n" +> carriageReturnMany
        , carriageReturnMany +> t "\n"
        , t "\n" +> carriageReturnMany
        , t "\n" ]

carriageReturn     = rule "CarriageReturn" [ t "\r" ]
carriageReturnMany = rule "CarriageReturnMany"
                   [ carriageReturn +> carriageReturnMany
                   , carriageReturn ]

horizontalTabulationCharacter = rule "HorizontalTabulationCharacter" [ t "\t" ]
verticalTabulationCharacter   = rule "VerticalTabulationCharacter"   [ t "\v" ]

formFeed = rule "FormFeed" [ t "\f" ]
