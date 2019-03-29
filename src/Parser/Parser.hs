module Parser.Parser ( parse
                     , module Parser.Combinators.Base ) where

import Parser.Combinators.Base
-- NOTE!
-- 1. if there will be extra combinators, the parse function should support any
-- parser, so Parser.Combinators.Extra must be imported anyways;
import Parser.General.Memo

parse :: Parser -> String -> ParserRes
parse p "" = error "Input string must be non empty!"
parse p i  = res
             where res   = fst pPair
                   pPair = p empty i
