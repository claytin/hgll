module Parser.Generator.Exception (GenExcpt(..)) where

import Control.Exception.Base
import Data.Typeable

data GenExcpt = GenFailure deriving (Show, Read, Typeable, Eq)

instance Exception GenExcpt
