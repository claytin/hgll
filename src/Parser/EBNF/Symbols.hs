module Parser.EBNF.Symbols (letter) where

import Parser.Combinators.Base

letter =  t "a" <|> t "b" <|> t "c" <|> t "d" <|> t "e" <|> t "f" <|> t "g"
      <|> t "h" <|> t "i" <|> t "j" <|> t "k" <|> t "l" <|> t "m" <|> t "n"
      <|> t "o" <|> t "p" <|> t "q" <|> t "r" <|> t "s" <|> t "t"
      <|> t "u" <|> t "v" <|> t "w" <|> t "x" <|> t "y" <|> t "z"
      <|> t "A" <|> t "B" <|> t "C" <|> t "D" <|> t "E" <|> t "F" <|> t "G"
      <|> t "H" <|> t "I" <|> t "J" <|> t "K" <|> t "L" <|> t "M" <|> t "N"
      <|> t "O" <|> t "P" <|> t "Q" <|> t "R" <|> t "S" <|> t "T"
      <|> t "U" <|> t "V" <|> t "W" <|> t "X" <|> t "Y" <|> t "Z"

decimalDigit =  t "0" <|> t "1" <|> t "2" <|> t "3" <|> t "4"
            <|> t "5" <|> t "6" <|> t "7" <|> t "8" <|> t "9"

concatenateSymbol = t ","

alternativeSymbol = t "|" <|> t "/" <|> t "!"

startGroupSymbol  = t "("
startOptionSymbol = t "[" <|> t "(/"
startRepeatSymbol = t "{" <|> t "(:"

endGroupSymbol  = t ")"
endOptionSymbol = t "]" <|> t "/)"
endRepeatSymbol = t "}" <|> t ":)"

singleQuoteSymbol = t "'"
doubleQuoteSymbol = t "\""

repeatSymbol = t "*"

terminatorSymbol = t ";" <|> t "."

otherCharacter =  spaceCharacter
              <|> t ":" <|> t "+" <|> t "_" <|> t "%" <|> t "@" <|> t "&"
              <|> t "#" <|> t "$" <|> t "<" <|> t ">" <|> t "\\" <|> t "~"
              <|> t "`" <|> t "~"

spaceCharacter = t " "

horizontalTabulationCharacter = t "\t"
verticalTabulationCharacter   = t "\v"

formFeed = t "\f"

newLine =  t "\n"
       <|> s "NewLine" [ carriageReturnMany, t "\n" ]
       <|> s "NewLine" [ t "\n", carriageReturnMany ]
       <|> s "NewLine" [ carriageReturnMany, t "\n", carriageReturnMany ]

carriageReturn     = t "\r"
carriageReturnMany = s "CarriageReturnMany"
    [ carriageReturn , carriageReturnMany ]
