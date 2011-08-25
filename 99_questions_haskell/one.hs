module Main where

-- Find the last element of a list
myLast :: [a] -> a
myLast = head . reverse

-- Find the last but one element of a list
myButLast :: [a] -> a
myButLast = head . tail . reverse

-- Find the Kth element of a list with the first element being 1
elementAt :: [a] -> Int -> a
elementAt xs n = xs !! (n - 1)

-- Now do it without using !!
elementAt' :: [a] -> Int -> a
elementAt' _ 0 = error "Index out of bounds"
elementAt' [] _ = error "Index out of bounds"
elementAt' (x:xs) 1 = x
elementAt' (x:xs) n = elementAt' xs (n - 1)

-- find the number of elements in a list
myLength :: [a] -> Int
myLength = foldr (\_ n -> n + 1) 0
