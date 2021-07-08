--- This module contains the implementation of
--- a direct mapping from hypergraphs to Grappa parsers.
--- This shows that parsers resemble grammars very closely.
--- This principle is used in the actual BPMN to BPEL transformation
--- implemented in the module `BPMN2BPEL`.

{-# OPTIONS_CYMAKE -Wno-overlapping #-}

module BPMNParser where

import Prelude hiding ( (*>) )

import Grappa


process :: Grappa String ()
process = edge ("start", [n1]) *> 
          flow (n1,n2) *> 
          edge ("end", [n2])
          where n1,n2 free

flow :: (Node,Node) -> Grappa String ()
flow (n1,n2) = flElem (n1,n2) 
flow (n1,n2) = flElem (n1,n) *> 
               flow (n,n2)
               where n free
              
flElem :: (Node,Node) -> Grappa String ()
flElem (n1,n2) = edge ("act", [n1,n2])
flElem (n1,n2) = edge ("inter", [n1,n2])
flElem (n1,n2) = edge ("pgw", [n1,n1t,n1r,n1b]) *>
                 flow (n1t,n2t) *>
                 edge ("pgw", [n2l,n2t,n2,n2b]) *>
                 flow (n1b,n2b) 
                 where n1t,n1r,n1b,n2l,n2t,n2b free
flElem (n1,n2) = edge ("xgw", [n1,n1t,n1r,n1b]) *>
                 flow (n1t,n2t) *>
                 edge ("xgw", [n2l,n2t,n2,n2b]) *>
                 flow (n1b,n2b) 
                 where n1t,n1r,n1b,n2l,n2t,n2b free

---------------------------------------------------------------------------
--- An example of a small hypergraph.
ex_sm :: Graph String
ex_sm = [("start", [1]), ("pgw", [1,2,4,3]), ("act", [2,6]),
         ("act", [3,7]), ("pgw", [5,6,8,7]), ("end", [8])]

-- BPMNParser> :add Grappa
-- BPMNParser Grappa> (process <* eoi) ex_sm
-- ((),[])
-- BPMNParser Grappa> (process <* eoi) [e1,e2,e3] where e1,e2,e3 free
-- {e1=("start",[_a]), e2=("act",[_a,_b]), e3=("end",[_b])} ((),[])
-- ...
