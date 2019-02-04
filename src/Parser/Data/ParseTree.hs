module Parser.Data.ParseTree ( ParseTree(..)
                             , Label ) where

-- Some aliases, cause readability
type Label = String
type Tk    = String -- Tk stands for token

-- A parse tree is composed of the following elements
-- Eps:  empty;
-- Leaf: a node that can only be a terminal;
-- Node: an inner node; labeled by a rule of the grammar and has n >= 0
--       children, sub-trees
data ParseTree = Eps
               | Leaf Tk
               | Node Label [ ParseTree ]

instance Ord ParseTree where
    -- Eps cases
    compare Eps Eps = EQ
    compare _   Eps = GT -- any production or token is greater than Eps
    compare Eps _   = LT -- duh!
    -- Leaf cases
    compare (Node _ _) (Leaf _)   = GT -- a rule is greater than a token
    compare (Leaf _)   (Node _ _) = LT -- duh!
    compare (Leaf tk)  (Leaf tk') = compare tk tk'
    -- Node case
    -- * Two productions are equal if all its sub-trees are equal, notice that
    --   there is no comparison of labels, as long as the trees have the same
    --   structure we are fine, variables can always be renamed
    compare (Node _ (t:ts)) (Node _ (t':ts')) = case compare t t' of
        EQ -> compare ts ts'
        GT -> GT -- "first match ordering"; somewhat lexicographic
        LT -> LT -- same

instance Eq ParseTree where
    t == t' = case compare t t' of
        EQ -> True
        _  -> False

-- Pretty printing
-- Don't worry about it, there is nothing pretty about pretty printing

-- Utility show functions
indent   :: Int -> String
indent n = (concat . replicate n) "  "

align   :: Int -> String
align n = newLine ++ indent n
          where newLine = "\n"

-- The structure of the definitions of indentShow and show hopefully depicts
-- how the tree is displayed
indentShow          :: Int -> [ParseTree] -> String
indentShow n [ ]    = ""
indentShow n (x:xs) = case x of
    (Node l c) -> align n ++ "[." ++ l
                          ++ indentShow (n + 1) c ++ " ]"
                          ++ indentShow n xs
    _          -> align n ++ show x ++ indentShow n xs

instance Show ParseTree where
    show Eps        = "[ ]"
    show (Leaf tk)  = tk
    show (Node l c) = "[." ++ l
                           ++ indentShow 1 c ++ " ]"
