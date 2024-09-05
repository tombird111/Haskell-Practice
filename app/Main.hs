module Main where

import System.Environment
import Compiler

--Task 3.4
main :: IO ()
main = do input <- getLine
          putStrLn(show (ccomp (read input)))