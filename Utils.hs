module Utils where

-- This library contains all kinds of different functions.
-- Some of them may at some point be extracted into libraries
-- of their own, deleted, re-introduced, expanded upon,
-- reduced, or anything else.

import Data.Char (isDigit)
import Data.List (group, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
--import Control.Parallel.Strategies (parMap, rseq)

-------------
-- Reading --
-------------

readInt     = (read :: String -> Int)
readInteger = (read :: String -> Integer)
readDouble  = (read :: String -> Double)

readDigits     :: String -> [Int]
readDigits      = map (readInt . (:[]))

dropNondigits   = dropWhile (not . isDigit)
takeDigits      = takeWhile isDigit
takeDigitsMinus = takeWhile (\ c -> isDigit c || c == '-')

------------
-- Tuples --
------------

tuplify2 [a, b]       = (a, b)
tuplify3 [a, b, c]    = (a, b, c)
tuplify4 [a, b, c, d] = (a, b, c, d)

listify2 (a, b)       = [a, b]
listify3 (a, b, c)    = [a, b, c]
listify4 (a, b, c, d) = [a, b, c, d]


fst3 (a, _, _) = a
snd3 (_, b, _) = b
thi3 (_, _, c) = c

fst4 (a, _, _, _) = a
snd4 (_, b, _, _) = b
thi4 (_, _, c, _) = c
fou4 (_, _, _, d) = d


-----------
-- Lists --
-----------

-- Fast counting function based on Maps, requires Ord
ordCounts :: Ord a => [a] -> Map a Int
ordCounts = Map.fromListWith (+) . map (,1)

-- Return only those list elements that appear exactly once.
-- Obviously requires Eq, but not Ord, and is stable.
singletons :: Eq a => [a] -> [a]
singletons []     = []
singletons (x:xs)
    | x `notElem` xs = x : singletons xs
    | otherwise      = singletons $ filter (/=x) xs

-- The same, but based on Data.Map. Significantly faster, but
-- requiring Ord, and not stable.
ordSingletons :: Ord a => [a] -> [a]
ordSingletons = map fst . Map.toList . Map.filter (==1) . ordCounts


-- Based on a list of Bools, keep elements of a given list or not.
keepP :: [Bool] -> [a] -> [a]
keepP bs = dropP (map not bs)

-- Based on a list of Bools, drop elements of a given list or not.
dropP :: [Bool] -> [a] -> [a]
dropP [] [] = []
dropP (b:bs) (x:xs)
    | b         = dropP bs xs
    | otherwise = x : dropP bs xs


-- Apply a function to one single element of a list, specified by index
mapAt :: Int -> (a -> a) -> [a] -> [a]
mapAt i f xs = take i xs ++ [f (xs !! i)] ++ drop (i + 1) xs

-- A faster nub, which, however, requires an order and is unstable.
-- O(n*log n) instead of O(n^2).
sortNub :: Ord a => [a] -> [a]
sortNub = map head . group . sort

-- Another one, using Set
setNub :: Ord a => [a] -> [a]
setNub = Set.toList . Set.fromList

-- And using Set to only find the number of unique elements
countUnique :: Ord a => [a] -> Int
countUnique = Set.size . Set.fromList


-- Cartesian product of two lists
cart :: [a] -> [b] -> [(a,b)]
cart x y = (,) <$> x <*> y

-- Like dropWhile, but also drops the first element
-- that does not fulfill the predicate any more.
dropWhile1 :: Eq a => (a -> Bool) -> [a] -> [a]
dropWhile1 p = drop 1 . dropWhile p


-- Just like takeWhile, but include the first element
-- that failed the check
takeWhile1 :: Eq a => (a -> Bool) -> [a] -> [a]
takeWhile1 _ []     = []
takeWhile1 p (x:xs)
    | p x       = x : takeWhile1 p xs
    | otherwise = [x]

-- Find the first recurring element and return
-- the index of its first and its second occurrence
firstRecur :: Ord a => [a] -> Maybe (Int, Int)
firstRecur = go Map.empty 0
  where
    go _ _ []        = Nothing
    go seen i (x:xs) = case (Map.lookup x seen) of
                             Just j  -> Just (j,i)
                             Nothing -> go (Map.insert x i seen) (i+1) xs

-- Group a list into a list of sublist of length n.
-- The last sublist may be shorter than n.
groupn :: Int -> [a] -> [[a]]
groupn _ [] = []
groupn n xs = (take n xs) : groupn n xs'
  where
    xs' = drop n xs

-- Tokenise an array into a list of arrays based on delimiters
-- Multiple delimiters are considered as one delimiter.
-- Delimiters at the beginning and end are ignored
tok :: Eq a => [a] -> [a] -> [[a]]
tok _ [] = []
tok delims input@(x:xs)
    | isDelimiter x = tok delims xs
    | otherwise     = curToken : tok delims input'
  where
    isDelimiter = (`elem` delims)
    curToken    = takeWhile (not . isDelimiter) input
    input'      = dropWhile isDelimiter $ dropWhile (not . isDelimiter) input

-- 2d map, i.e. apply a function to all elements of all lists inside a list.
map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f = map (map f)

-- Insert an element into a list at the given position.
push :: Int -> a -> [a] -> [a]
push i x xs = left ++ [x] ++ right
  where
    left  = take i xs
    right = drop i xs

-- Remove the i-th element of a given list xs.
pop :: Int -> [a] -> [a]
pop i xs = (take i xs) ++ (drop (i + 1) xs)

-- Count how many element of a list fulfil a predicate.
countTrue :: (a -> Bool) -> [a] -> Int
countTrue p = length . filter p

-- Count how often a list occurs within another list.
countOcc :: Eq a => [a] -> [a] -> Int
countOcc _ [] = 0
countOcc [] _ = error "Utils.countOcc: Empty search list."
countOcc ndl hay
    | length ndl > length hay      = 0
    | ndl == take (length ndl) hay = 1 + countOcc ndl (tail hay)
    | otherwise                    = countOcc ndl (tail hay)

nubByWith :: (a -> a -> Bool) -> ([a] -> a) -> [a] -> [a]
nubByWith eq agg = go
  where
    go []          = []
    go elems@(x:_) = agg (filter (\ e -> x `eq` e) elems)
                   : go (filter (\ e -> not (x `eq` e)) elems)

----------------------------
-- Simple Parallelisation --
----------------------------

-- Syntactic sugar 
--pmap :: (a -> b) -> [a] -> [b]
--pmap = parMap rseq

-- Assuming the evaluation function is costly,
-- we need to execute it in parallel.
--pfilter :: (a -> Bool) -> [a] -> [a]
--pfilter p = concat . pmap (\ x -> if p x then [x] else [])



