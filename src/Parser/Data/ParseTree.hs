module Parser.Data.ParseTree ( ParseTree(..)
                             , Label ) where

-- A parse tree is composed of at least one of the following elements
-- Eps:   represents an empty production;
-- Token: it is a leaf node that corresponds to a grammar terminal. The
--        parameter for this constructor is a string Tk;
-- Seq:   a constructor that has no direct correspondent on grammars or parse
--        trees, it is sort of an intermidiary that allows the existence of
--        labeled rules. A grammar rule is composod by at least one sequence of
--        n >= 1 juxtapostion of elements, this juxtapostion is represented by
--        the Seq constructor. Seq is a binary sub-tree with (L)eft and (R)ight
--        branches, the recursive structure of Seq allows for arbitrarily big
--        sequencies, building an unbaleced ParseTree;
-- Rule:  corresponds to the parsing of one of the (Alt)ernatives of the
--        grammar rule identified by Label. It a acts as a sort of container
--        for the elements of ParseTree.
data ParseTree = Eps
               | Token Tk
               | Seq   L R
               | Rule  Label Alt
               deriving (Ord, Eq)
-- where
type Tk    = String    -- Tk stands for token
type L     = ParseTree
type R     = ParseTree
type Alt   = ParseTree
type Label = String

{-- Pretty printing;
 -- Don't worry about it, there is nothing pretty about pretty printing --}

instance Show ParseTree where
    show r = show' 0 r ++ "\n"

show'              :: Int -> ParseTree -> String
show' n Eps        = align n ++ "[ ]"
show' n (Token tk) = align n ++ tk
show' n (Seq t t') = show' n t ++ show' n t'
show' n (Rule l t) = align n ++ "[." ++ l
                                     ++ show' (n + 1) t ++ " ]"

-- Utility functions
indent   :: Int -> String
indent n = (concat . replicate n) "  " -- two white spaces for each level of
                                       -- indentation

align   :: Int -> String
align n = "\n" ++ indent n
