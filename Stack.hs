-- A simple Stack type.

module Stack where

type Stack a = [a]

empty :: Stack a
empty = []

push :: a -> Stack a -> Stack a
push x xs = x : xs

pop :: Stack a -> (Maybe a, Stack a)
pop []     = (Nothing, [])
pop (x:xs) = (Just x, xs)

peek :: Stack a -> Maybe a
peek []    = Nothing
peek (x:_) = Just x

isEmpty :: Stack a -> Bool
isEmpty [] = True
isEmpty _  = False

size :: Stack a -> Int
size = length

clear :: Stack a -> Stack a
clear _ = []

swap :: Stack a -> Stack a
swap []         = []
swap (x:[])     = [x]
swap (x1:x2:xs) = (x2:x1:xs)



