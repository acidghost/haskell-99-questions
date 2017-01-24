import System.Random (randomRIO, randomRs, getStdGen, StdGen)
import Control.Monad (replicateM)
import Data.List (nub, sortBy, sortOn, groupBy)
import Data.Function (on)

import qualified Problems11To20 as Previous


-- 21. Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt elm list pos
    | pos > length list + 1 || pos < 1 = error "Position not valid"
    | pos == length list + 1 = list ++ [elm]
    | otherwise = insertAt' list 1 []
    where
        insertAt' list@(x:xs) n acc
            | n == pos = acc ++ [elm] ++ list
            | otherwise = insertAt' xs (n+1) (acc ++ [x])


-- 22. Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range a b = [a..b]


-- 23. Extract a given number of randomly selected elements from a list.
rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect _ 0 = return []
rndSelect list n
    | n < 0 = error "Cannot extract a negative number of items"
    | otherwise = do
        randomInts <- replicateM n $ randomRIO (0, (length list) - 1)
        return $ take n [list !! x | x <- randomInts]

-- Random sample without replacement.
rndSelect' :: [a] -> Int -> IO [a]
rndSelect' list n = rndSelect'' list [] n
    where
        rndSelect'' l acc count
            | count > length l = error "Cannot extract more items than the list has"
            | count < 0 = error "Cannot extract a negative number of items"
            | count == 0 = return acc
            | otherwise = do
                k <- randomRIO (0, (length l) - 1)
                rndSelect'' (Previous.removeAt k l) (acc ++ [l !! k]) (count - 1)


-- 24. Lotto: Draw N different random numbers from the set 1..M.
-- Using previous function (without replacement version).
permutation :: Int -> Int -> IO [Int]
permutation n m = rndSelect' [1..m] n
-- Using `nub` and default StdGen.
permutation' :: Int -> Int -> IO [Int]
permutation' n m = take n . nub . randomRs (1, m) <$> getStdGen
-- Using `nub` and esternal StdGen.
permutation'' :: Int -> Int -> StdGen -> [Int]
permutation'' n m = take n . nub . randomRs (1, m)


-- 25. Generate a random permutation of the elements of a list.
rndPermu :: [a] -> IO [a]
rndPermu l = do
    randomInts <- permutation (length l) (length l)
    return [l !! (i - 1) | i <- randomInts]

rndPermu' :: [a] -> StdGen -> [a]
rndPermu' l gen = [l !! (i - 1) | i <- permutation'' (length l) (length l) gen]


-- 26. Generate the combinations of K distinct objects chosen from
-- the N elements of a list.
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [xs !! i : x | i <- [0..(length xs) - 1]
                                 , x <- combinations (n-1) (drop (i+1) xs)]
-- Version that returns also the items not selected in each combination.
combinations' :: Int -> [a] -> [([a], [a])]
combinations' 0 xs = [([], xs)]
combinations' _ [] = []
combinations' n (x:xs) = ts ++ ds
    where ts = [ (x:ys, zs) | (ys, zs) <- combinations' (n-1) xs ]
          ds = [ (ys, x:zs) | (ys, zs) <- combinations'  n    xs ]
-- Helper function to pretty-print combinations.
printCombinations :: (Show a) => [a] -> IO ()
printCombinations = putStrLn . foldl1 (\acc s -> acc ++ "\n" ++ s) . map show
-- The number of combinations should be equal to the binomial coefficient.
-- Note: factorials with and without tail recursion.
factorial n = if n == 1 || n == 0 then 1 else n * factorial (n - 1)
factorial' n = factorial'' 1 1
    where factorial'' p c = if c > n then p else factorial'' (c * p) (c + 1)
binCoeff n r = (factorial n) / ((factorial r) * factorial (n-r))


-- 27. Group the elements of a set into disjoint subsets.
-- a) In how many ways can a group of 9 people work in 3 disjoint subgroups
-- of 2, 3 and 4 persons? Write a function that generates all the possibilities
-- and returns them in a list.
group3 :: [a] -> [[[a]]]
group3 [] = [[]]
group3 list = [ x1:x2:x3:[] | (x1, rs1) <- combinations' 2 list
                            , (x2, rs2) <- combinations' 3 rs1
                            , (x3, _)   <- combinations' 4 rs2 ]
-- The following is used to test the preceding function.
lenGroup3 n = (binCoeff n 2) * (binCoeff (n-2) 3) * (binCoeff (n-2-3) 4)
-- b) Generalize the above predicate in a way that we can specify a list of
-- group sizes and the predicate will return a list of groups.
group :: [a] -> [Int] -> [[[a]]]
group [] _ = [[]]
group xs (n:ns) = [ g:gs | (g, rs) <- combinations' n xs
                         ,  gs     <- group rs ns ]
-- The following is used to test the preceding function.
lenGroup _ [] = 1
lenGroup ln (n:ns) = (binCoeff ln n) * lenGroup (ln-n) ns


-- 28. Sorting a list of lists according to length of sublists
-- a) We suppose that a list contains elements that are lists themselves.
-- The objective is to sort the elements of this list according to their length.
-- E.g. short lists first, longer lists later, or vice versa.
-- We could use normal filter but doing so requires one additional list-pass
-- per call to the `lsort'` helper function.
filter' :: (a -> Bool) -> [a] -> ([a], [a])
filter' _ [] = ([], [])
filter' predicate xs = filter'' xs ([], [])
    where filter'' [] res = res
          filter'' (x:xs) (ok, nok)
            | predicate x = filter'' xs (ok ++ [x], nok)
            | otherwise   = filter'' xs (ok, nok ++ [x])
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort xs = lsort' xs 1
    where lsort' [] _ = []
          lsort' xs n =
              let (ok, nok) = filter' (\x -> length x == n) xs
              in ok ++ lsort' nok (n+1)
lsort' :: [[a]] -> [[a]]
lsort' = sortOn length
lsort'' :: [[a]] -> [[a]]
lsort'' = sortBy (compare `on` length)
-- b) Again, we suppose that a list contains elements that are lists themselves.
-- But this time the objective is to sort the elements of this list according to
-- their length frequency; i.e., in the default, where sorting is done
-- ascendingly, lists with rare lengths are placed first, others with a more
-- frequent length come later.
lfsort :: [[a]] -> [[a]]
lfsort lists = concat groups
    where groups = lsort $ groupBy equalLength $ lsort lists
          equalLength x y = (length x) == (length y)
lfsort' :: [[a]] -> [[a]]
lfsort' = concat . lsort . groupBy ((==) `on` length) . lsort
