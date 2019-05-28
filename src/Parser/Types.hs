-- TODO!
-- 1. Add new comment for the Parser type;

-- This module allows for a better separation of "concerns"
module Parser.Types ( module Parser.Data.ParseTree
                    , Parser(..)
                    , Res
                    , Input ) where

import Parser.Data.ParseTree

-- About the Parser type.
-- Add comment here ...
newtype Parser a = Parser (Input -> [Res a])
-- where
type Res a  = (a, Input)
type Input  = String
