import Parser.Combinators.Base

sbin  = rule "SBIN" [ op +> bin ]

bin   = rule "BIN"
      [ digit +> bin
      , digit ]

op    = rule "OP" [ t "-" , t "+" ]

digit = rule "DIGIT" [ t "0" , t "1" ]
