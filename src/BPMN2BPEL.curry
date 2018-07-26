--- This module defines a bi-directional BPMN to BPEL transformation

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns -Wno-overlapping #-}

module BPMN2BPEL where

import Grappa
import Prelude hiding ((<$>), (<$))


--- The type of BPMN edges.
data BPMNComp =
     BPMNStart | BPMNEnd | BPMNPGW |
     BPMNXGW String String |        --XOR conditions as params
     BPMNActivity String |          --label as param
     BPMNInter BPMNInterKind String --kind and label as params

data BPMNInterKind = BPMNWait | BPMNReceive

--- An exampe BPMN graph.
ex :: Graph BPMNComp
ex = [(BPMNStart,[1]),(BPMNPGW,[1,2,4,3]),(BPMNActivity "act1",[2,6]),
      (BPMNActivity "act2",[6,10]),(BPMNPGW,[15,16,18,17]),
      (BPMNActivity "act3",[3,5]),(BPMNXGW "cond1" "cond2",[5,7,9,8]),
      (BPMNActivity "act4",[7,11]),(BPMNActivity "act5",[8,14]),
      (BPMNInter BPMNWait "ev2",[11,13]),(BPMNXGW "" "",[12,13,17,14]),
      (BPMNInter BPMNReceive "ev1",[10,16]),(BPMNEnd,[18])]


-------------------------------------------------------------------------
-- A significant subset of BPEL:

--- The type of activities of a BPEL process.
data BPELComp = Invoke String | Wait String | Receive String |
                Flow BPEL BPEL | Switch String String BPEL BPEL

--- Sequences of activities of a BPEL process.
type BPEL = [BPELComp]

--- The main parsing operation.
--- The parser works on typed hypergraphs with edges of type `BPMNComp`
--- and returns a semantic value of type `BPEL` (the BPEL corresponding
--- to the input BPMN).
processS :: Grappa BPMNComp BPEL
processS = edge (BPMNStart,[n1]) *>
           flowS (n1,n2) <*
           edge (BPMNEnd,[n2])
 where n1,n2 free

flowS :: (Node,Node) -> Grappa BPMNComp BPEL
flowS (n1,n2) = (:[]) <$> flElemS (n1,n2)
flowS (n1,n2) = (:)   <$> flElemS (n1,n) <*> flowS (n,n2)
                where n free

flElemS :: (Node,Node) -> Grappa BPMNComp BPELComp
flElemS (n1,n2) = translateInter itype name <$
                  edge (BPMNInter itype name, [n1,n2])
                  where itype,name free
                        translateInter BPMNWait    = Wait
                        translateInter BPMNReceive = Receive
flElemS (n1,n2) = Invoke lab <$
                  edge (BPMNActivity lab, [n1,n2])
                  where lab free
flElemS (n1,n2) = edge (BPMNPGW, [n1,n1t,n1r,n1b]) *>
                  (Flow <$>
                   flowS (n1t,n2t) <*>
                   flowS (n1b,n2b)) <*
                  edge (BPMNPGW, [n2l,n2t,n2,n2b])
                  where n1t,n1r,n1b,n2l,n2t,n2b free
flElemS (n1,n2) = edge (BPMNXGW c1 c2, [n1,n1t,n1r,n1b]) *>
                  (Switch c1 c2 <$>
                   flowS (n1t,n2t) <*>
                   flowS (n1b,n2b)) <*
                   edge (BPMNXGW d1 d2, [n2l,n2t,n2,n2b])
                  where c1,c2,d1,d2,n1t,n1r,n1b,n2l,n2t,n2b free



--- Shows a BPEL as a string in XML format.
bpel2xml :: BPEL -> String
bpel2xml f = "<process>\n" ++
                 indent 1 (seq2xml f) ++
             "</process>\n"

seq2xml :: [BPELComp] -> String
seq2xml [e] = bpelComp2xml e
seq2xml es@(_:_:_) = "<sequence>\n" ++
                         indent 1 (concatMap bpelComp2xml es) ++
                     "</sequence>\n"

bpelComp2xml :: BPELComp -> String
bpelComp2xml (Invoke name) = "<invoke name=\""++name++"\"/>\n"
bpelComp2xml (Wait name) = "<wait name=\""++name++"\"/>\n"
bpelComp2xml (Receive name) = "<receive name=\""++name++"\"/>\n"
bpelComp2xml (Flow f1 f2) = "<flow>\n" ++
                                indent 1 (seq2xml f1) ++
                                indent 1 (seq2xml f2) ++
                            "</flow>\n"
bpelComp2xml (Switch c1 c2 f1 f2) =
                            "<switch>\n" ++
                            " <case cond=\""++c1++"\">\n" ++
                                indent 1 (seq2xml f1) ++
                            " </case>\n" ++
                            " <case cond=\""++c2++"\">\n" ++
                                indent 1 (seq2xml f2) ++
                            " </case>\n" ++
                            "</switch>\n"

indent :: Int -> String -> String
indent n s = unlines (map (nblanks++) (lines s))
 where nblanks = take n (repeat ' ')

{-
Usage:

Transform our example graph into BPEL:

BPMN2BPEL> processS ex
([Flow [Invoke "act1",Invoke "act2",Receive "ev1"] [Invoke "act3",Switch "cond1" "cond2" [Invoke "act4",Wait "ev2"] [Invoke "act5"]]],[])

Generating graphs:

BPMN2BPEL Grappa> (processS <* eoi) [e1,e2,e3] where e1,e2,e3 free

Reverse the transformation:

BPMN2BPEL> processS [e1,e2,e3,e4] =:= ([Invoke "act1",Wait "ev1"],[])  where e1,e2,e3,e4 free
{e1=(BPMNStart,[_a]), e2=(BPMNActivity "act1",[_a,_b]), e3=(BPMNInter BPMNWait "ev1",[_b,_c]), e4=(BPMNEnd,[_c])} True

BPMN2BPEL> processS [e1,e2,e3,e4,e5,e6] =:= ([Switch "c1" "c2" [Invoke "act1"] [Invoke "act2"]],[]) where e1,e2,e3,e4,e5,e6 free
{e1=(BPMNStart,[_a]), e2=(BPMNXGW "c1" "c2",[_a,_b,_c,_d]), e3=(BPMNActivity "act1",[_b,_e]), e4=(BPMNActivity "act2",[_d,_f]), e5=(BPMNXGW _g _h,[_i,_e,_j,_f]), e6=(BPMNEnd,[_j])} True

-}
