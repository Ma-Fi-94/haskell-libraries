module FloydWarshall where

-- This implementation is due to
-- https://stackoverflow.com/questions/65872551/floyd-warshall-algorithm-in-haskell

import Data.List (transpose)

-- A weight. Using the right order, we can derive Ord for free.
data Weight = Finite Int | Infinity deriving (Eq, Ord, Show)

-- Unbox a finite weight.
unboxWeight :: Weight -> Int
unboxWeight (Finite w) = w
unboxWeight _          = error "Cannot unbox infinite weight."

-- Helper for addition.
addWeights :: Weight -> Weight -> Weight
addWeights (Finite x) (Finite y) = Finite (x + y)
addWeights _ _                   = Infinity

-- The main function just steps the matrix a number of times equal to
-- the node count.  Also pass along k at each step, since the step
-- function requires it.
floydwarshall :: [[Weight]] -> [[Weight]]
floydwarshall m = snd (iterate step (0, m) !! length m)

-- One step. Takes k and the matrix for k, returns k+1 and the matrix for
-- k+1.
step :: (Int, [[Weight]]) -> (Int, [[Weight]])
step (k, m) = (k + 1, zipWith (stepRow ktojs) istok m)
  where
    ktojs = m !! k            -- current k to each j
    istok = transpose m !! k  -- each i to current k

-- Make shortest paths from one i to all j.
-- We need the shortest paths from the current k to all j
-- and the shortest path from this i to the current k
-- and the shortest paths from this i to all j
stepRow :: [Weight] -> Weight -> [Weight] -> [Weight]
stepRow ktojs itok itojs = zipWith stepOne itojs ktojs
  where
    stepOne itoj ktoj = itoj `min` (itok `addWeights` ktoj)
