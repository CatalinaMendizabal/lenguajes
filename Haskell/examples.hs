module Examples where

doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = x * 2 + y * 2

doubleSmallNumber :: (Num a, Ord a) => a -> a
doubleSmallNumber x = (if x > 100 then x else x * 2) + 1

-- ['w', 'o'] ++ ['o', 't'] ==> "woot"
-- 'A' : " Small cat" ==> "A Small Cat"
-- "Steve Buscemi" !! 6 ==> saca el 6to elemento "B"

-- head [5,4,3,2,1] == 5 || last [5,4,3,2,1] == 1
-- tail [5,4,3,2,1] == [4,3,2,1] || init [5,4,3,2,1] == [5,4,3,2]

-- reverse, take cant list, drop cant list -> saca los cant primero numeros y devuelve le resto
-- maximum && minimum ==> para listas

-- elem ==> 4 `elem` [3,4,5,6] == True

-- [x * 2 | x <- [1..10], x*2 >= 12]

-- TUPLAS:
-- fst y snd
-- zip [1,2,3,4,5] [5,5,5,5,5] = [(1,5), (2,5), (3,5), (4,5), (5,5)]


-- 1) last element of a list

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- 2) find the last but one element of a list --> ante ultimo element
myButLast :: [a] -> a
myButLast = last . init -- agarro todos menos el ultimo y despues saco el ultimo

-- 3) encontrar el nth elemento de una lista

elementAt :: [a] -> Int -> a
elementAt list index
    | index == 0 = head list
    | otherwise = list !! (index - 1)

-- 4) find the number of elements of a list

myLength :: [a] -> Int
myLength = foldr (\ x -> (+) 1) 0
-- myLength [] = 0
-- myLength (_:xs) = 1 + myLength xs


-- 5) reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 6) Check if it is palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs
 -- [] = True
 -- [_] = True
 -- xs = head xs == tail xs && (isPalindrome init xs tail xs)


 -- 7) flatten a nested list structuee (a, (b, (c, d)), e) => [a, b, c, d, e]
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- 8) eliminate consecutive duplicates of list elems

-- 9) pack (a, a, b, c, c , a, a , a) = ((A,A) (B) (C, C) (A, A,A))
packList :: (Eq a) => [a] -> [[a]]
packList [] = []
packList (x:xs) = dups : packList rest
    where (dups, rest) = span (==x) (x:xs)

-- 10) run length encoding
runLength :: (Eq a) => [a] -> [(a, Int)]
runLength [] = []
runLength (x:xs) = (head (head (packList (x:xs))), length (head (packList (x:xs)))) : runLength [y | y <- xs, y /= x]

-- 11) modify run-length encoding
data RunLengthElement a = Single a | Tuple (a, Int) deriving (Show)

modifiedRunLength :: (Eq a) => [a] -> [RunLengthElement a]
modifiedRunLength [] = []
modifiedRunLength (x:xs)
    | l == 1 = Single e : modifiedRunLength xs
    | otherwise = Tuple (e, l) : modifiedRunLength [y | y <- xs, y /= x]
    where 
        e = head (head (packList (x:xs)))
        l = length (head (packList (x:xs)))