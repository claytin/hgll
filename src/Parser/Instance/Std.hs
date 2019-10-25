-- TODO!
-- 1. Add some comments
module Parser.Instance.Std ( module Parser.Traits
                           , Std(..)
                           , Res(..)) where

import Parser.Traits

-- About the Parser type.
-- Add comment here ...
newtype Std a = Std (String -> [Res a])
-- where
type Res a = (String, a)

instance Parser Std where
    parse (Std p) = p

---- Applicative m => Monad (m :: * -> *)
instance Monad Std where
    -- return :: a -> Std a
    return a    = Std $ \i -> [(i, a)]
    -- (>>=) :: Std a -> (a -> Std b) -> Std b
    Std p >>= f = Std $ \i ->
        concat [ q i' | (i', a) <- p i , let Std q = f a ]

instance GrammO Std where
    -- (<:>) :: Std a -> Std b -> Std (a, b)
    p <:> q = do a <- p
                 b <- q
                 return (a, b)
    -- (<|>) :: Std a -> Std a -> Std a
    p <|> q = Std $ \i -> parse p i ++ parse q i

-- Functor f => Applicative (f :: * -> *)
instance Applicative Std where
    -- pure :: a -> Std a
    pure = return
    -- (<*>) :: Std (a -> b) -> Std a -> Std b
    fs <*> ps = do f <- fs
                   a <- ps
                   return (f a)

instance Functor Std where
    -- fmap :: (a -> b) -> Std a -> Std b
    fmap f p = p >>= \a -> return (f a)
