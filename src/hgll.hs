-- TODO
-- 1. Add a help function

module Main where

import System.Environment
import Parser.Parser

data Mode = Parse | Gen deriving (Show)

main :: IO ()
main = do args <- getArgs
          --
          let mode = if length args == 0 then
                         error "No argumets, try hgll help; aborting!"
                     else
                         switch (args !! 0)
          --
          case mode of
               Parse -> mainParse (tail args)
               Gen   -> mainGen (tail args)

switch :: String -> Mode
switch mode
    | mode == "parse" = Parse
    | mode == "gen"   = Gen
    | otherwise       = error $ "First argument must be a selector, either " ++
                                "\"parse\" or \"gen\"!"

mainParse      :: [String] -> IO ()
mainParse args = putStrLn "parse"

mainGen      :: [String] -> IO ()
mainGen args = putStrLn "gen"
