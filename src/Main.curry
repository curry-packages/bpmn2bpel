--- This main module contains an example of the BPMN to BPEL transformation.

module Main where

import Control.Search.AllValues ( getOneValue )

import Grappa
import BPMN2BPEL

--- Applies the parser `BPMNBPEL.processS` to the example hypergraph
--- `BPMNBPEL.ex` and pretty-prints the resulting BPEL representation in XML:
main :: IO ()
main = do
  mbv <- getOneValue (processS ex)
  case mbv of
    Nothing -> putStrLn "No solution"
    Just v  -> let bpel = getSemRep v
               in putStrLn (bpel2xml bpel)

