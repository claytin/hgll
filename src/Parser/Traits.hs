module Parser.Traits where

class Parser m where
    parse         :: m a -> String -> [(String, a)]
    fullParse     :: m a -> String -> [a]
    fullParse p i = [ a | ([], a) <- parse p i ]

class GrammO m where
    (<:>) :: m a -> m b -> m (a, b)
    (<|>) :: m a -> m a -> m a

infixl 3 <:>
infixl 2 <|>
