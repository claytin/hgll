import Parser.Combinators.Base

sbin  = s "SBIN" [op, bin]
bin   = s "BIN"  [digit, bin] >|< digit
op    = token "-" >|< token "+"
digit = token "0" >|< token "1"
