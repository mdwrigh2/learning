module Main where

import qualified Random as R

-- 1. Find the last element of a list
myLast :: [a] -> a
myLast = head . reverse

-- 2. Find the last but one element of a list
myButLast :: [a] -> a
myButLast = head . tail . reverse

-- 3. Find the Kth element of a list with the first element being 1
elementAt :: [a] -> Int -> a
elementAt xs n = xs !! (n - 1)

-- Now do it without using !!
elementAt' :: [a] -> Int -> a
elementAt' _ 0 = error "Index out of bounds"
elementAt' [] _ = error "Index out of bounds"
elementAt' (x:xs) 1 = x
elementAt' (x:xs) n = elementAt' xs (n - 1)

-- 4. find the number of elements in a list
myLength :: [a] -> Int
myLength = foldr (\_ n -> n + 1) 0

-- 5. Reverse a list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse xs) ++ [x]

-- 6. Find whether a list is a palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- 7. Flatten a list
   -- Note that in this one, I'm ignorning the possibility that lists are nested, since nested lists
   -- are not possible in Haskell without defining a new, recursive datatype.
flatten :: [[a]] -> [a]
flatten = foldr (++) []

-- 8. Remove consecutive, duplicate elements from a list
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) = if x == y
                     then compress $ x:xs
                     else x:(compress $ y:xs)
-- 9. Pack consecutive duplicates of list elements into sublists.
pack :: Eq a => [a] -> [[a]]
pack (x:xs) = reverse $ foldl packify [[x]] xs
  where packify (y:ys) x | x == head y = (x:y):ys
                         | otherwise = [x]:y:ys

-- 10. Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N E) where N is the number of duplicates of the element E.
encode :: Eq a => [a] -> [(Int, a)]
encode  = map (\x -> (length x, head x)) . pack 

-- 11. Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists. This requires a new data type in Haskell, since lists have to be homogeneous.
data RunLengthEncoding a = Single a | Multiple a Int
  deriving (Show, Eq, Ord)
encodeModified :: Eq a => [a] -> [RunLengthEncoding a]
encodeModified = map encodify . pack
  where encodify [x] = Single x
        encodify xs = Multiple (head xs) (length xs)
-- 12. Decode a run-length encoded list.
decode :: [RunLengthEncoding a] -> [a]
decode = foldr decode' []
  where decode' (Multiple item num) xs = (replicate num item) ++ xs 
        decode' (Single item) xs = [item] ++ xs
-- Now doing it with a map just because
decode' :: [RunLengthEncoding a] -> [a]
decode' = flatten . map decodify
  where decodify (Single item) = [item]
        decodify (Multiple item num) = replicate num item

-- 13. Run-length encoding of a list (direct solution).
encodeDirect :: Eq a => [a] -> [RunLengthEncoding a]
encodeDirect = map encodify . foldr count []
  where count item [] = [(item, 1)]
        count item (x@(val, num):xs) | val == item = (val, num + 1):xs
                                     | otherwise   = (item, 1):x:xs
        encodify (item, 1)   = Single item
        encodify (item, num) = Multiple item num

-- 14. Duplicate the elements of a list
duplicate :: [a] -> [a]
duplicate = foldr (\x xs -> x:x:xs) []

-- 15. Replicate the elements of a list a given number of times
replicate' :: [a] -> Int -> [a]
replicate' xs num = foldr (\y ys -> (replicate num y) ++ ys) [] xs

-- 16. Drop every N'th element from a list
dropN :: [a] -> Int -> [a]
dropN xs n = map snd $ filter (\x -> fst x `mod` n /= 0) $ zip [1..] xs

-- 17. Split a list into two parts; the length of the first part is given.
  -- Don't use built-in functions.
split :: [a] -> Int -> ([a], [a])
split xs     0 = ([], xs)
split []     _ = error "Index out of bounds"
split (x:xs) n = let (xs', ys') = split xs (n - 1) in (x:xs', ys')

-- 18. Extract a slice from a list. Indexes are inclusive.
slice :: [a] -> Int -> Int -> [a]
slice xs low high = take (high - low + 1) $ drop low xs

-- 19. Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate xs num = let n = num `mod` length xs in
                    if n >= 0
                       then (drop n xs) ++ (take n xs)
                       else (drop (length xs - n) xs) ++ 
                            (take (length xs - n) xs)

-- 20. Remove the K'th element from a list
remove :: [a] -> Int -> (a, [a])
remove (x:xs) 0 = (x, xs)
remove []     _ = error "Index out of bounds"
remove (x:xs) k = let (x', xs') = remove xs $ k-1 in (x', x:xs')

-- 21. Insert an element at a given position into a list.
insertAt :: a -> Int -> [a] -> [a]
insertAt item 0 xs            = item:xs
insertAt item location []     = error "Index out of bounds"
insertAt item location (x:xs) | location < 0 = error "Index out of bounds"
                              | otherwise    = x:(insertAt item (location - 1) xs)

-- 22. Create a list containing all integers within a given range
range low high = [low..high]

-- 23. Extract a given number of randomly selected elements from a list.
{-randSelect :: Int -> [a] -> IO [a]-}

genUniqRandNum :: Int -> (Int, Int) -> IO [Int]
genUniqRandNum 0 _ = return []
genUniqRandNum num (low, high) | num > (high - low + 1) =
  error "Selected more unique random numbers than possible for the given range."
                               | otherwise = do
  genUniqRandNum' num (low,high) $ return []

genUniqRandNum' :: Int -> (Int, Int) -> IO [Int] -> IO [Int]
genUniqRandNum' num (low, high) xs = do
  rand <- R.getStdRandom $ R.randomR (low, high)
  nums <- xs
  if rand `elem` nums
     then genUniqRandNum' num (low, high) (return nums)
     else if length (rand:nums) == num
             then return (rand:nums)
             else genUniqRandNum' num (low, high) (return $ rand:nums)

randSelect :: Int -> [a] -> IO [a]
randSelect num xs = select (genUniqRandNum num (0, length xs)) xs
  where select ys xs = fmap (map (xs !!)) ys
