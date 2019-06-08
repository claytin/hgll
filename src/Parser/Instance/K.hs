-- TODO!
-- 1. Add some comments

-- RankNTypes are necessary for the quantification of the type variable b in
-- the definition of the newtype KP m a
{-# LANGUAGE RankNTypes #-}

module Parser.Instance.K ( module Parser.Traits
                         , KP(..) ) where

import Parser.Traits

-- About the Parser type.
-- Add comment here ...
newtype KP m a = KP (forall b. (a -> m b) -> m b)

instance (Monad m, Parser m) => Parser (KP m) where
    parse (KP p) i = parse (p return) i

-- Applicative m => Monad (m :: * -> *)
instance Monad (KP m) where
    -- return :: a -> (KP m) a
    return v   = KP $ \k -> k v
    -- (>>=) (KP m) a -> (a -> (KP m) b) -> (KP m) b
    KP p >>= f = KP $ \k -> p (\a -> let KP q = f a in q k)

instance GrammO m => GrammO (KP m) where
    -- (<:>) :: (KP m) a -> (KP m) b -> (KP m) (a, b)
    --KP p <:> KP q = KP $ \k -> p k <:> q k
    p <:> q = do a <- p
                 b <- q
                 return (a, b)
    -- (<|>) :: (KP m) a -> (KP m) a -> (KP m) a
    KP p <|> KP q = KP $ \k -> p k <|> q k

-- Functor f => Applicative (f :: * -> *)
instance Applicative (KP m) where
    -- pure :: a -> (KP m) a
    pure = return
    -- (<*>) :: (KP m) (a -> b) -> (KP m) a -> (KP m) b
    fs <*> ps = do f <- fs
                   a <- ps
                   return (f a)

instance Functor (KP m) where
    -- fmap :: (a -> b) -> (KP m) a -> (KP m) b
    fmap f p = p >>= \a -> return (f a)
