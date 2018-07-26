--- This module defines the basic data structures for
--- representing graphs and hypergraphs and the corresponding
--- graph parsers.

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns -Wno-overlapping #-}

module Grappa where

import Prelude hiding ((<$>), (<$))

--- All nodes are numbered.
type Node = Int

--- A type for the edge labels `t` can be passed as a type parameter.
--- In the simplest case this can be just strings representing their labels.
--- The tentacle numbers correspond to the position of a node in the
--- list of incident nodes.
type Edge t = (t, [Node])

--- A graph is declared as a list of edges each with its incident nodes
type Graph t = [Edge t]

--- The type of a graph parser. It is parameterized over the
--- type `res` of semantic values associated to graphs.
type Grappa t res = Graph t -> (res, Graph t)

--- Access the semantic representation of the result of a graph parser.
getSemRep :: (res, Graph t) -> res
getSemRep (r,_) = r

-- Some primitives for the construction of graph parsers.

--- Given an arbitrary value `v`, `pSucceed` always succeeds
--- returning `v` as a result without any consumption of the input graph `g`.
pSucceed :: res -> Grappa t res
pSucceed v g = (v, g)

--- `eoi` succeeds only if the graph is already completely consumed.
--- In this case, the result `()` is returned
eoi :: Grappa t ()
eoi [] = ((), [])

--- The parser `edge e` succeeds only if the given edge `e` is part of the
--- given input graph `g`.
edge :: Edge t -> Grappa t ()
edge e g | g=:=(g1++e:g2) = ((), g1++g2)
 where g1, g2 free

--- The choice between two parsers.
(<|>) :: Grappa t res -> Grappa t res -> Grappa t res
(p1 <|> _ ) g = p1 g
(_  <|> p2) g = p2 g

--- The successive application of two parsers where the result
--- is constructed by function application.
(<*>) :: Grappa t (r1->r2) -> Grappa t r1 -> Grappa t r2
(p <*> q) g = case p g of
               (pv, g1) -> case q g1 of
                             (qv, g2) -> (pv qv, g2)

(<$>) :: (res1->res2) -> Grappa t res1 -> Grappa t res2
f <$> p = pSucceed f <*> p

(<$)  :: res1 -> Grappa t res2 -> Grappa t res1
f <$ p = const f <$> p

(<*)  :: Grappa t res1 -> Grappa t res2 -> Grappa t res1
p <* q = (\x _ -> x) <$> p <*> q

(*>)  :: Grappa t res1 -> Grappa t res2 -> Grappa t res2
p *> q = (\_ x -> x) <$> p <*> q


--- The combinator `chain p (n1,n2)` can be used to identify
--- a non-empty chain of graphs that can be parsed with `p`.
--- This chain has to be anchored between the nodes `n1` and `n2`.
chain :: ((Node,Node) -> Grappa t a) -> (Node,Node) -> Grappa t [a]
chain p (n1,n2) = (:[]) <$> p (n1,n2)
chain p (n1,n2) = (:)   <$> p (n1,n) <*> chain p (n,n2)
 where n free
