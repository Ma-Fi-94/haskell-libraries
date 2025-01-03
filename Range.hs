module Range where

-- A closed range of arbitrary type with both borders inclusive.
data Range a = R a a deriving (Eq, Show)

-- Get low end of a Range
low :: Range a -> a
low (R a _) = a

-- Get high end of a Range
high :: Range a -> a
high (R _ b) = b

-- Check whether a value is contained in a Range.
isin :: Ord a => a -> Range a -> Bool
isin x (R a b) = x >= a && x <= b

-- Check if the left range completely contains the right one. 
contains :: Ord a => Range a -> Range a -> Bool
R a b `contains` R c d = c >= a && d <= b

-- Check if there is any overlap between two ranges.
overlaps :: Ord a => Range a -> Range a -> Bool
overlaps (R a b) (R c d)
    | a <= c && b >= c = True
    | c <= a && d >= a = True
    | otherwise        = False

-- Given two ranges, compute their union. Returns a list of ranges,
-- since the union of two disjunct ranges will be two ranges.
union :: Ord a => Range a -> Range a -> [Range a]
union r1@(R a b) r2@(R c d)
    | r1 `contains` r2 = [r1]
    | r2 `contains` r1 = [r2]
    | r1 `overlaps` r2 = [R (minimum [a, c]) (maximum [b, d])]
    | otherwise        = [r1, r2]

-- Given two ranges, compute their intersection. Returns a
-- Maybe Range, s.t. Nothing can be returned for empty intersections.
intersect :: Ord a => Range a -> Range a -> Maybe (Range a)
intersect r1@(R a b) r2@(R c d)
    | r1 `contains` r2 = Just r2
    | r2 `contains` r1 = Just r1
    | r1 `overlaps` r2 = Just $ R (maximum [a, c]) (minimum [b, d])
    | otherwise        = Nothing

-- Given a list of disjunct Ranges rs, add another Range r, returning
-- a list of Ranges guaranteed to be disjunct again.
-- Three cases are possible:
-- (a) The new range r is contained in one of the ranges rs.
--     Then, we do not need to add it.
-- (b) The new range r does not intersect with any of rs.
--     In that case, we simply add r to the list.
-- (c) The new range r intersects with at least one of rs.
--     Then, we retain all unaffected ranges (that don't
--     intersect with r), and merge the new range r with
--     all the affected ranges to a new Range [a, b].
disjunctJoin :: Ord a => [Range a] -> Range a -> [Range a]
disjunctJoin rs r
    | any (`contains` r) rs              = rs
    | all ((==Nothing) . intersect r) rs = r : rs
    | otherwise                          = new : unaffected
  where
    unaffected = filter ((==Nothing) . intersect r) rs
    affected   = filter ((/=Nothing) . intersect r) rs
    new        = R a b
    a          = minimum . map low  $ (r : affected)
    b          = maximum . map high $ (r : affected)
