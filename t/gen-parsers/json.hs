json = "json" =|> element

value = "value" =|> object
                <|> array
                <|> string
                <|> number
                <|> t "true"
                <|> t "false"
                <|> t "null"

object = "object" =|> t "{" # members # t "}"
                  <|> t "{" # ws # t "}"

members = "members" =|> member # t "," # members
                    <|> member

member = "member" =|> ws # string # ws # t ":" # element

array = "array" =|> t "[" # elements # t "]"
                <|> t "[" # ws # t "]"

elements = "elements" =|> element # t "," # elements
                      <|> element

element = "element" =|> ws # value # ws

string = "string" =|> t "\"" # characters # t "\""

characters = "characters" =|> character # characters <|> eps

character = "character" =|> t "a" <|> t "b" <|> t "c" <|> t "d" <|> t "e"
                        <|> t "f" <|> t "g" <|> t "h" <|> t "i" <|> t "j"
                        <|> t "k" <|> t "l" <|> t "m" <|> t "n" <|> t "o"
                        <|> t "p" <|> t "q" <|> t "r" <|> t "s" <|> t "t"
                        <|> t "u" <|> t "v" <|> t "w" <|> t "x" <|> t "y"
                        <|> t "z"
                        <|> t "A" <|> t "B" <|> t "C" <|> t "D" <|> t "E"
                        <|> t "F" <|> t "G" <|> t "H" <|> t "I" <|> t "J"
                        <|> t "K" <|> t "L" <|> t "M" <|> t "N" <|> t "O"
                        <|> t "P" <|> t "Q" <|> t "R" <|> t "S" <|> t "T"
                        <|> t "U" <|> t "V" <|> t "W" <|> t "X" <|> t "Y"
                        <|> t "Z"
                        <|> digit
                        <|> t "\x0020"
                        <|> t "!" <|> t "#" <|> t "$" <|> t "%" <|> t "&"
                        <|> t "'" <|> t "(" <|> t ")" <|> t "*" <|> t "+"
                        <|> t "," <|> t "-" <|> t "." <|> t "/" <|> t ":"
                        <|> t ";" <|> t "<" <|> t "=" <|> t ">" <|> t "?"
                        <|> t "@" <|> t "[" <|> t "]" <|> t "{" <|> t "}"
                        <|> t "|" <|> t "~"
                        <|> t "\x007f"
                        <|> t "\\" # escape

escape = "escape" =|> t "\"" <|> t "\\" <|> t "/" <|> t "b" <|> t "f" <|> t "n"
                  <|> t "r" <|> t "t"
                  <|> t "u" # hex # hex # hex

hex = "hex" =|> digit
            <|> t "a" <|> t "b" <|> t "c" <|> t "d" <|> t "e" <|> t "f"
            <|> t "A" <|> t "B" <|> t "C" <|> t "D" <|> t "E" <|> t "F"

number = "number" =|> integer # fraction # exponent

integer = "integer" =|> t "-" # onenine # digits
                    <|> onenine # digits
                    <|> t "-" # digit
                    <|> digit

digits = "digits" =|> digit # digits
                  <|> digit

digit = "digit" =|> t "0" <|> onenine

onenine = "onenine" =|> t "1" <|> t "2" <|> t "3" <|> t "4" <|> t "5" <|> t "6"
                    <|> t "7" <|> t "8" <|> t "9"

fraction = "fraction" =|> t "." # digits <|> eps

exponent = "exponent" =|> t "e" # sign # digits
                      <|> t "E" # sign # digits
                      <|> eps

sign = "sign" =|> t "+" <|> t "-" <|> eps

ws = "ws" =|> t "\x0020" # ws
          <|> t "\n" # ws
          <|> t "\r" # ws
          <|> t "\t" # ws
          <|> eps
