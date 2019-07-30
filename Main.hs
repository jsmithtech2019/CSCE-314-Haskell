
-- CSCE 314 Sections 501, 502
-- Assignment 4 Part II

module Main where

  import System.Environment (getArgs)
  import Control.Monad (when)

  import W
  import WParser

  main = do
    args <- getArgs -- get the command line arguments
  
    let (first:rest) = args
    let debug = first == "-d"
    let fileName = if debug then head rest else first
  
    str <- readFile fileName
  
    let prog = parse wprogram str 
  
    let ast = case prog of 
                [(p, [])] -> p
                [(_, inp)] -> error ("Unused program text: " 
                                     ++ take 256 inp) -- this helps in debugging
                [] -> error "Syntax error"
                _ -> error "Ambiguous parses"
    result <- exec ast []
  
    when debug $ print "AST:" >> print prog
    when debug $ print "RESULT:" >> print result


