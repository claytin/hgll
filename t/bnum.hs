import Parser.Parser

sbin  = s "SBIN" [op, bin]
bin   = s "BIN"  [digit, bin] >|< digit
op    = t "-" >|< t "+"
digit = t "0" >|< t "1"
