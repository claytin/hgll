letter = "letter" =|> t "a" <|> t "b" <|> t "c" <|> t "d" <|> t "e" <|> t "f"
                  <|> t "g" <|> t "h" <|> t "i" <|> t "j" <|> t "k" <|> t "l"
                  <|> t "m" <|> t "n" <|> t "o" <|> t "p" <|> t "q" <|> t "r"
                  <|> t "s" <|> t "t" <|> t "u" <|> t "v" <|> t "w" <|> t "x"
                  <|> t "y" <|> t "z"
                  <|> t "A" <|> t "B" <|> t "C" <|> t "D" <|> t "E" <|> t "F"
                  <|> t "G" <|> t "H" <|> t "I" <|> t "J" <|> t "K" <|> t "L"
                  <|> t "M" <|> t "N" <|> t "O" <|> t "P" <|> t "Q" <|> t "R"
                  <|> t "S" <|> t "T" <|> t "U" <|> t "V" <|> t "W" <|> t "X"
                  <|> t "Y" <|> t "Z"

decimalDigit = "decimalDigit" =|> t "0" <|> t "1" <|> t "2" <|> t "3" <|> t "4"
                              <|> t "5" <|> t "6" <|> t "7" <|> t "8" <|> t "9"

concatenateSymbol     = "concatenateSymbol"     =|> t ","
definingSymbol        = "definingSymbol"        =|> t "="
alternativeSymbol     = "alternativeSymbol"     =|> t "|" <|> t "/" <|> t "!"
endGroupSymbol        = "endGroupSymbol"        =|> t ")"
endOptionSymbol       = "endOptionSymbol"       =|> t "]" <|> t "/)"
endRepeatSymbol       = "endRepeatSymbol"       =|> t "}" <|> t ":)"
firstQuoteSymbol      = "firstQuoteSymbol"      =|> t "'"
repetitionSymbol      = "repetitionSymbol"      =|> t "*"
secondQuoteSymbol     = "secondQuoteSymbol"     =|> t "\""
exceptSymbol          = "exceptSymbol"          =|> t "-"
specialSequenceSymbol = "specialSequenceSymbol" =|> t "?"
startGroupSymbol      = "startGroupSymbol"      =|> t "("
startOptionSymbol     = "startOptionSymbol"     =|> t "[" <|> t "(/"
startRepeatSymbol     = "startRepeatSymbol"     =|> t "{" <|> t "(:"
terminatorSymbol      = "terminatorSymbol"      =|> t ";" <|> t "."

otherCharacter = "otherCharacter" =|> spaceCharacter
                                  <|> t ":" <|> t "+" <|> t "_" <|> t "%"
                                  <|> t "@" <|> t "&" <|> t "#" <|> t "$"
                                  <|> t "<" <|> t ">" <|> t "^" <|> t "`"
                                  <|> t "~" <|> t "\\"

spaceCharacter = "spaceCharacter" =|> t "\x0020"

horizontalTabulationCharacter = "horizontalTabulationCharacter" =|> t "\t"
verticalTabulationCharacter   = "verticalTabulationCharacter"   =|> t "\v"

formFeed       = "formFeed"       =|> t "\f"
carriageReturn = "carriageReturn" =|> t "\r"

newLine = "newLine" =|>
    closure (carriageReturn) # t "\n" # closure (carriageReturn)

terminalCharacter = "terminalCharacter" =|> letter
                                        <|> decimalDigit
                                        <|> concatenateSymbol
                                        <|> definingSymbol
                                        <|> alternativeSymbol
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

terminalString = "terminalString" =|>
        firstQuoteSymbol # firstTerminalCharacter
     #  closure (firstTerminalCharacter) # firstQuoteSymbol
    <|> secondQuoteSymbol # secondTerminalCharacter
     #  closure (secondTerminalCharacter) # secondQuoteSymbol

firstTerminalCharacter  = "firstTerminalCharacter"  =|>
    terminalCharacter -. firstQuoteSymbol
secondTerminalCharacter = "secondTerminalCharacter" =|>
    terminalCharacter -. secondQuoteSymbol

integer                 = "integer" =|> decimalDigit # closure (decimalDigit)
metaIdentifier          = "metaIdentifier" =|>
    letter # closure (metaIdentifierCharacter)
metaIdentifierCharacter = "metaIdentifierCharacter" =|> letter <|> decimalDigit

syntax     = "syntax"     =|> syntaxRule # closure (syntaxRule)

syntaxRule = "syntaxRule" =|>
    metaIdentifier # definingSymbol # definitionsList # terminatorSymbol

definitionsList  = "definitionsList"  =|>
    singleDefinition # closure (alternativeSymbol # singleDefinition)

singleDefinition = "singleDefinition" =|>
    syntacticFactor # closure (concatenateSymbol # syntacticFactor)

syntacticFactor  = "syntacticFactor"  =|>
    opt (integer # repetitionSymbol) # syntacticPrimary

syntacticPrimary = "syntacticPrimary" =|> optionalSequence
                                      <|> repeatedSequence
                                      <|> groupedSequence
                                      <|> metaIdentifier
                                      <|> terminalString
                                      <|> emptySequence

optionalSequence = "optionalSequence" =|>
    startOptionSymbol # definitionsList # endOptionSymbol
repeatedSequence = "repeatedSequence" =|>
    startRepeatSymbol # definitionsList # endRepeatSymbol
groupedSequence  = "groupedSequence"  =|>
    startGroupSymbol # definitionsList # endGroupSymbol
emptySequence = "emptySequence" =|> eps
