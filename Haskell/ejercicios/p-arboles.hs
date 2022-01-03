module Practice where

filter' :: Ord a => [a] -> (a -> Bool) -> [a]
filter' [] _ = []
filter' (x:xs) f = if f x then x : filter' xs f else filter' xs f

data Tree a = Node a (Tree a) (Tree a) | Nil deriving Show

leaf :: a -> Tree a
leaf a = Node a Nil Nil

insert :: Ord a => Tree a -> a -> Tree a
insert Nil a = leaf a
insert (Node e l r) a
    | a < e = Node e (insert l a) r
    | a > e = Node e l (insert r a)
    | otherwise = Node a l r

instance (Eq a) => Eq (Tree a) where
    Nil == Nil = True
    (Node a l r) == (Node a' l' r') = a == a' && l == l' && r == r'
    _ == _ = False


inorder :: Show a => Tree a -> String
inorder Nil = ""
inorder (Node a Nil Nil) = show a
inorder (Node a Nil r) = show a ++ ", " ++ inorder r
inorder (Node a l Nil) = inorder l ++ ", " ++ show a
inorder (Node a l r) = inorder l ++ ", " ++ show a ++ ", " ++ inorder r

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [z | z <- xs, z >= x]