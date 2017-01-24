import Data.List (group)


-- 31. Determine whether a given integer number is prime.
isPrime :: Int -> Bool
isPrime n = if n < 2 then False else isPrime' 2
    where isPrime' m
            | n == m = True
            | n `rem` m /= 0 = isPrime' (m+1)
            | otherwise = False


-- 32. Determine the greatest common divisor of two positive integer numbers.
-- Use Euclid's algorithm.
myGCD :: Int -> Int -> Int
myGCD x y = if y == 0 then x else myGCD y (x `rem` y)


-- 33. Determine whether two positive integer numbers are coprime.
-- Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Int -> Int -> Bool
coprime x y = myGCD x y == 1


-- 34. Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number of
-- positive integers r (1 <= r < m) that are coprime to m.
totient :: Int -> Int
totient m = length $ filter (coprime m) [1..m-1]


-- 35. Determine the prime factors of a given positive integer.
-- Construct a flat list containing the prime factors in ascending order.
primeFactors :: Int -> [Int]
primeFactors m
    | m < 1 = error "Only positive numbers are allowed"
    | otherwise = primeFactors' m 2 []
    where
        primeFactors' n d acc
            | n == 1 = acc
            | n `rem` d == 0 = primeFactors' (n `div` d) d (acc ++ [d])
            | otherwise = primeFactors' n (d+1) acc


-- 36. Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors and their multiplicity.
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map tupled . group . primeFactors
    where tupled xs@(x:_) = (x, length xs)


-- 37. Calculate Euler's totient function phi(m) (improved).
-- See problem 34 for the definition of Euler's totient function.
-- If the list of the prime factors of a number m is known in the form of
-- problem 36 then the function phi(m) can be efficiently calculated as follows:
-- Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their
-- multiplicities) of a given number m. Then phi(m) can be calculated with the
-- following formula:
-- phi(m) = (p1 - 1) * p1 ** (m1 - 1) *
--          (p2 - 1) * p2 ** (m2 - 1) *
--          (p3 - 1) * p3 ** (m3 - 1) * ...
-- Note that a ** b stands for the b'th power of a.
totient' :: Int -> Int
totient' = foldr operation 1 . primeFactorsMult
    where operation (p, m) acc = acc * (p - 1) * p ^ (m - 1)
totient'' :: Int -> Int
totient'' m = product [ (p - 1) * p ^ (m - 1) | (p, m) <- primeFactorsMult m ]


-- 38. Compare the two methods of calculating Euler's totient function.
-- Use the solutions of problems 34 and 37 to compare the algorithms.
-- Take the number of reductions as a measure for efficiency. Try to calculate
-- phi(10090) as an example.
--
-- (no solution required)


-- 39. A list of prime numbers.
-- Given a range of integers by its lower and upper limit, construct a list of
-- all prime numbers in that range.
primesR :: Int -> Int -> [Int]
primesR a b = filter isPrime [a..b]


-- 40. Goldbach's conjecture.
-- Goldbach's conjecture says that every positive even number greater than 2 is
-- the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most
-- famous facts in number theory that has not been proved to be correct in the
-- general case. It has been numerically confirmed up to very large numbers
-- (much larger than we can go with our Prolog system). Write a predicate to
-- find the two prime numbers that sum up to a given even integer.
goldbach :: Int -> (Int, Int)
goldbach m = head [ (x, y) | x <- ps, y <- ps, x + y == m ]
    where ps = primesR 2 m


-- 41. Given a range of integers by its lower and upper limit, print a list of
-- all even numbers and their Goldbach composition.
-- In most cases, if an even number is written as the sum of two prime numbers,
-- one of them is very small. Very rarely, the primes are both bigger than
-- say 50. Try to find out how many such cases there are in the range 2..3000.
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = map goldbach $ filter even [a..b]
goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b mv = filter (\(x, y) -> x > mv && y > mv) (goldbachList a b)
