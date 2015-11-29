import Data.List
import qualified Problems1To10 as Previous

-- 11. Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists.
data ListItem a = Single a | Multiple Int a
    deriving (Show)
encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified xs = 
    map (\x -> 
        if length x == 1 
        then Single (head x) 
        else Multiple (length x) (head x)
    ) (group xs)
-- or
encodeModified' :: (Eq a) => [a] -> [ListItem a]
encodeModified' = map encodeHelper . Previous.encode
    where
        encodeHelper (1, x) = Single x
        encodeHelper (n, x) = Multiple n x

-- 12. Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version.
decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
    where
        decodeHelper (Single x) = [x]
        decodeHelper (Multiple n x) = replicate n x

-- 13. Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them.
-- As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect = map helper . group
    where
        helper [x] = Single x
        helper xs = Multiple (length xs) (head xs)

-- 14. Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : (dupli xs)

-- 15. Replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (replicate n x) ++ (repli xs n)
-- or without replicate
repli' [] _ = []
repli' (x:xs) n = (helper x n) ++ (repli' xs n)
    where
        helper _ 0 = []
        helper x n = [x] ++ (helper x (n-1))

-- 16. Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = helper xs n n
    where
        helper [] _ _ = []
        helper (x:xs) n 1 = helper xs n n
        helper (x:xs) n i = x : (helper xs n (i-1))

-- 17. Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split list@(x:xs) n
    | n > 0 = (x : ys, zs)
    | otherwise = ([], list)
    where
        (ys, zs) = split xs (n-1)

-- 18. Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included).
-- Start counting the elements with 1.
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) i j
    | i > 1 = slice xs (i-1) (j-1)
    | i == 1 && j > 0 = x : (slice xs i (j-1))
    | otherwise = []

-- 19. Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
rotate :: [a] -> Int -> [a]
rotate = error "Implement me!"
