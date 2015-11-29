import Data.List

-- 1. Find the last element of a list
myLast :: [a] -> a
myLast [] = error "Empty list given"
myLast [e] = e
myLast (_:tail) = myLast tail

-- 2. Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "Empty list given"
myButLast [x] = error "Singleton list given"
myButLast (_:tail) = if length tail == 2 then head tail else myButLast tail

-- 3. Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Index out of bound"
elementAt (x:_) 1 = x
elementAt (_:xs) k
	| k < 1 = error "Index out of bound"
	| otherwise = elementAt xs (k - 1)

-- 4. Find the number of elements of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 5. Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- 6. Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = x == (last xs) && (isPalindrome $ init xs)

