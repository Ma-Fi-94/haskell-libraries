module WDGraph where

-- This library implements a finite weighted directed graph (WDGraph)
-- and some standard methods for it.

import Control.Arrow (second)
import Data.List ((\\), nub, sortOn)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)

-- For Dijkstra, we need our own data type to also allow for
-- infinite values. Ordering of constructors allows to derive
-- Ord for free.
data Distance = Finite Int | Infinity deriving (Eq, Ord, Show)

-- We only provide an implementation for addition of finite
-- numbers, because we won't need anything else.
instance Num Distance where Finite a + Finite b = Finite (a + b)

-- We represent the graph as a Map that links every
-- integer node ID to a list of weighted edges.
-- For now, we only allow for integer weights.
type Weight  = Int
type Node    = Int
type Edge    = (Node, Weight)
type WDGraph = Map Node [Edge]

-- Sugar for construction from adjacency list
fromList :: [(Node, [Edge])] -> WDGraph
fromList = M.fromList

-- Helper to get all nodes of a graph
allNodes :: WDGraph -> [Node]
allNodes = M.keys

-- Helper to only get all outgoing neighbours of a given node.
neighbours :: WDGraph -> Node -> [Node]
neighbours graph node = map fst $ edges graph node

-- Get all outgoing edges of a given node.
edges :: WDGraph -> Node -> [Edge]
edges graph node = case node `M.lookup` graph of
    Just edges -> edges
    Nothing    -> []

-- Depth-first search, returning only whether we can
-- reach the finish from the start, or not.
dfs :: WDGraph -> Node -> Node -> Bool
dfs graph start finish = go [start] (S.fromList (allNodes graph))
  where
    go queue unvisiteds
        | null queue          = False
        | S.null unvisiteds   = False
        | finish `elem` queue = True
        | otherwise           = go queue' unvisiteds'
      where
        current     = head queue
        queue'      = tail queue ++ (neighbours graph current)
        unvisiteds' = S.delete current unvisiteds

-- Breadth-first search, as before. Here, the queue is a Set insead
-- of a list, so we exclude duplicates automatically.
bfs :: WDGraph -> Node -> Node -> Bool
bfs graph start finish = go (S.singleton start) (S.fromList (allNodes graph))
  where
    go :: Set Node -> Set Node -> Bool
    go currents unvisiteds
        | S.null currents            = False
        | S.null unvisiteds          = False
        | finish `S.member` currents = True
        | otherwise                  = go currents' unvisiteds'
      where
        -- there really needs to be a
        -- S.concatMap :: (a -> [a]) -> Set a -> Set a
        currents'   = S.fromList 
                    . concatMap (neighbours graph) 
                    $ S.toList currents
        unvisiteds' = unvisiteds `S.difference` currents

-- Given a starting node, we run the complete Dijkstra algorithm,
-- returning for every node the distance to the starting node,
-- as well as for every node its predecessor.
-- An infinite distance denotes the node is not reachable.
dijkstra :: WDGraph -> Node -> (Map Node Distance, Map Node Node)
dijkstra graph start = go unvisiteds0 predecessors0 distances0
  where
    -- Set up Dijkstra
    unvisiteds0   = S.fromList (allNodes graph)
    predecessors0 = M.empty
    distances0    = M.fromList
                  $ [(start, Finite 0)]
                 ++ [(node, Infinity) | node <- (allNodes graph) \\ [start]]

    -- The actual Dijkstra. We terminate if either no nodes are unvisited,
    -- or all remaining unvisited nodes have infinite distance.
    go unvisiteds predecessors distances
        | S.null unvisiteds                                              = (distances, predecessors)
        | all (==Infinity) . map (distances M.!) $ (S.toList unvisiteds) = (distances, predecessors)
        | otherwise                                                      = go unvisiteds' predecessors' distances'
      where
        -- We work on the unvisited node with the smallest distance.
        (curNode, curDist) = head
                           . sortOn snd
                           . map (\ node -> (node, distances M.! node))
                           $ S.toList unvisiteds

        -- Remove current record from list of unvisited records.
        unvisiteds' = S.delete curNode unvisiteds

        -- We identify all possible (unvisited) next steps we can take,
        -- as well as their distances when stepping through the current node.
        nextOptions = map    (\ (node, distance) -> (node, distance + curDist))
                    . filter (\ (node, distance) -> node `S.member` unvisiteds)
                    . map    (\ (node, weight)   -> (node, Finite weight))
                    $ edges graph curNode

        -- Now, we look at all next options. If we can reach one faster
        -- by stepping through the current node, then we will update
        -- its distance and set its predecessor to the current node.
        nodeUpdates = filter (\ (node, cumDistance) -> cumDistance < distances M.! node)
                    $ nextOptions

        -- Do the updates found previously.
        distances'    = foldl (\ someMap (node, cumDistance)
                                  -> M.insert node cumDistance someMap)
                              distances
                              nodeUpdates
        predecessors' = foldl (\ someMap (node, cumDistance)
                                  -> M.insert node curNode someMap)
                              predecessors
                              nodeUpdates