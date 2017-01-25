import Control.Monad (replicateM)


-- 46. Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2
-- (for logical equivalence) which succeed or fail according to the result of
-- their respective operations; e.g. and(A,B) will succeed, if and only if both
-- A and B succeed.
-- A logical expression in two variables can then be written as in the following
-- example: and(or(A,B),nand(A,B)).
-- Now, write a predicate table/3 which prints the truth table of a given logical
-- expression in two variables.
not' :: Bool -> Bool
not' True = False
not' False = True
and',or',nor',nand',xor',impl',equ' :: Bool -> Bool -> Bool
and' True True = True
and' _    _    = False
-- and a b = if a == b then a else False
or' False False = False
or' _     _     = True
-- or' a b = if a then True else if b then True else False
nor'  a b = not' $ or' a b
nand' a b = not' $ and' a b
xor' True  False = True
xor' False True  = True
xor' _     _     = False
impl' a b = (not' a) `or'` b
equ' True  True  = True
equ' False False = True
equ' _     _     = False
table :: (Bool -> Bool -> Bool) -> IO ()
table expr = mapM_ putStrLn [ show x ++ " " ++ show y ++ " " ++ show r
                                | x <- bs, y <- bs, let r = expr x y ]
    where bs = [True, False]


-- 47. Truth tables for logical expressions (2).
-- Continue problem P46 by defining and/2, or/2, etc as being operators. This
-- allows to write the logical expression in the more natural way, as in the
-- example: A and (A or not B). Define operator precedence as usual; i.e. as in
-- Java.
infixl 3 `equ'`
infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
-- "not" has fixity 9 by default


-- 48. Truth tables for logical expressions (3).
-- Generalize problem P47 in such a way that the logical expression may contain
-- any number of logical variables. Define table/2 in a way that
-- table(List,Expr) prints the truth table for the expression Expr, which
-- contains the logical variables enumerated in List.
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [ toStr as ++ " " ++ show (f as) | as <- args ]
    where
        args = replicateM n [True, False]
        toStr = unwords . map (\a -> show a ++ space a)
        space True  = "  "
        space False = " "


-- 49. Gray codes.
-- An n-bit Gray code is a sequence of n-bit strings constructed according to
-- certain rules. For example,
-- n = 1: C(1) = ['0','1'].
-- n = 2: C(2) = ['00','01','11','10'].
-- n = 3: C(3) = ['000','001','011','010','110','111','101','100'].
-- Find out the construction rules and write a predicate with the following
-- specification: % gray(N,C) :- C is the N-bit Gray code
gray :: Int -> [String]
gray 0 = [""]
gray n = foldr (\x acc -> ('0':x):('1':x):acc) [] $ gray (n - 1)
