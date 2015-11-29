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
