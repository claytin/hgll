module Parser.Generator.Util ( gTerm
                             , gOpt
                             , gClosure, gStar
                             , gTimes
                             , cQuoteSym
                             , cOSSeqSym
                             , cCSSeqSym
                             , cTermOp
                             , cAltOp
                             , cSeqOp
                             , cRuleDefOp
                             , cRuleTermSym
                             , ecRepOp
                             , ecOptOp
                             , ecClosureOp ) where

import Parser.Data.ParseTree

gTerm            :: ParseTree -> String
gTerm (Token tk) = tk

-- The first equation of the following functions (Rule ...) unpack the actual
-- structure of the tree and it is necessary because of the recursive pattern
-- described by them
gOpt     :: (ParseTree -> String) -> ParseTree -> String
gOpt g t = case t of
    (Rule "Optional" t') -> gOpt g t'
    Eps                  -> ""
    _                    -> g t

gClosure     :: (ParseTree -> String) -> ParseTree -> String
gClosure g t = case t of
    (Rule "Closure" t') -> gClosure g t'
    (Seq l r)           -> g l ++ gClosure g r
    Eps                 -> ""

gTimes       :: Int -> (ParseTree -> String) -> ParseTree -> String
gTimes 0 _ _ = ""
gTimes 1 g t = g t
gTimes n g t = case t of
    (Rule "Repetition" t') -> gTimes n g t'
    (Seq l r)              -> g l ++ gTimes (n - 1) g r

-- Combinator generation operators and symbols
cQuoteSym :: String
cQuoteSym = "\""

cOSSeqSym :: String
cOSSeqSym = "("

cCSSeqSym :: String
cCSSeqSym = ")"

cTermOp :: String
cTermOp = "t "

cAltOp :: String
cAltOp = " <|> "

cSeqOp :: String
cSeqOp = " # "

cRuleDefOp :: String
cRuleDefOp = " =|> "

cRuleTermSym :: String
cRuleTermSym = "\n"

ecRepOp :: String
ecRepOp = " *. "

ecOptOp :: String
ecOptOp = "opt "

ecClosureOp :: String
ecClosureOp = "closure "

-- Aliases --
gStar = gClosure
