module Vector where

data Vec a = V Int [a] deriving (Show, Eq)

----------------------------------
-- Construction and destruction --
----------------------------------

vec :: [a] -> Vec a
vec xs = V (length xs) xs

zeroes :: Int -> Vec Int
zeroes n = V n $ replicate n 0

zeroesLike :: Vec a -> Vec Int
zeroesLike (V n _) = zeroes n

elems :: Vec a -> [a]
elems (V _ xs) = xs

-----------------------------
-- Vector space operations --
-----------------------------

vadd :: Num a => Vec a -> Vec a -> Vec a
vadd (V i xs) (V j ys)
    | i /= j    = error "Vector.add: Incompatible vector lengths."
    | otherwise = V i $ zipWith (+) xs ys

vsub :: Num a => Vec a -> Vec a -> Vec a
vsub v1 v2 = vadd v1 $ smul (-1) v2

smul :: Num a => a -> Vec a -> Vec a
smul c (V i xs) = V i $ map (*c) xs

---------------------------
-- Some other operations --
---------------------------

-- Hadamard product (= Schur product = elementwise multiplication)
hadprod :: Num a => Vec a -> Vec a -> Vec a
hadprod (V i xs) (V j ys)
    | i /= j    = error "Vector.hadprod: Incompatible vector lengths."
    | otherwise = V i $ zipWith (*) xs ys

-- Dot product
dot :: Num a => Vec a -> Vec a -> a
dot (V i xs) (V j ys)
    | i /= j    = error "Vector.hadprod: Incompatible vector lengths."
    | otherwise = sum $ zipWith (*) xs ys

-- Mapping a function onto all elements of the vector
vmap :: (a -> b) -> Vec a -> Vec b
vmap f (V i xs) = V i (map f xs)

-- zipWith for two vectors
vzipWith :: (a -> b -> c) -> Vec a -> Vec b -> Vec c
vzipWith f (V i xs) (V j ys)
    | i /= j    = error "Vector.vzipWith: Incompatible lengths."
    | otherwise = V i (zipWith f xs ys)

---------------------
-- A partial order --
---------------------

vandZipWith_ :: (a -> a -> Bool) -> Vec a -> Vec a -> Bool
vandZipWith_ f v1 v2 = and $ elems (vzipWith f v1 v2)

eq :: Ord a => Vec a -> Vec a -> Bool
eq = vandZipWith_ (==)

lt :: Ord a => Vec a -> Vec a -> Bool
lt = vandZipWith_ (<)

le :: Ord a => Vec a -> Vec a -> Bool
le = vandZipWith_ (<=)

gt :: Ord a => Vec a -> Vec a -> Bool
gt = vandZipWith_ (>)

ge :: Ord a => Vec a -> Vec a -> Bool
ge = vandZipWith_ (>=)