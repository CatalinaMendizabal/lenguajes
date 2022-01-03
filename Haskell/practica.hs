module Practica where

{-| (9) Pack consecutive duplicates of list elements into sublists -}

packList :: (Eq a) => [a] -> [[a]]
packList [] = []
packList (x:xs) = dups : packList rest
    where (dups, rest) = span (==x) (x:xs)

{- | (10) Run-length encoding of a list.-}

runLength :: (Eq a) => [a] -> [(a, Int)]
runLength [] = []
runLength (x:xs) = (head (head (packList (x:xs))), length (head (packList (x:xs)))) : runLength [y | y <- xs, y /= x]

{- | (11) Modified run-length encoding.-}

data RunLengthElement a = Single a | Tuple (a, Int) deriving (Show)

modifiedRunLength :: (Eq a) => [a] -> [RunLengthElement a]
modifiedRunLength [] = []
modifiedRunLength (x:xs)
    | l == 1 = Single e : modifiedRunLength xs
    | otherwise = Tuple (e, l) : modifiedRunLength [y | y <- xs, y /= x]
    where 
        e = head (head (packList (x:xs)))
        l = length (head (packList (x:xs)))

{-| (29) Sorting a list of lists according to length of sublists.-}

lengthSort :: [[a]] -> [[a]]
lengthSort [] = []
lengthSort (x:xs) = lengthSort [y | y <- xs, length y < length x] ++ [x] ++ lengthSort [y | y <- xs, length y > length x]

-- Parcial
{-| Definir el tipo de datos Tree, con datos sólo en las hojas, e implementar la clase Eq -}

data Tree a = Empty | Branch (Tree a) (Tree a) | Leaf a

instance (Eq a) => Eq (Tree a) where
    Empty == Empty = True
    Branch l r == Branch l' r' = l == l' && r == r'
    Leaf a == Leaf b = a == b
    _ == _ = False

instance (Ord a) => Ord (Tree a) where
    Empty <= Empty = True
    Leaf x <= Leaf y = x <= y
    Branch l r <= Branch l' r' = l <= l' && r <= r'
    Leaf x <= Branch l r = Leaf x <= l && Leaf x <= r
    Leaf x <= Empty = False 
    _ <= _ = False

{-| 2- Implementar la generación del árbol de compresión de Huffman, esto implica el análisis
de repetición de cada letra, y posterior generación del árbol, el cual puede ser generado
balanceado o no. La que se recibe para generar el árbol es un String ([Char]) -}

data HuffmanTree = EmptyTree | Node Int HuffmanTree HuffmanTree | LeafH (Char, Int) deriving (Show)

freq :: [Char] -> [(Char, Int)]
freq [] = []
freq (x:xs) = (x, length [y | y <- x:xs, y == x]) : freq [y | y <- xs, y /= x]

sortTuple :: [(Char, Int)] -> [HuffmanTree]
sortTuple [] = []
sortTuple (x:xs) = sortTuple [y | y <- xs, snd y < snd x] ++ [LeafH x] ++ sortTuple [y | y <- xs, snd y >= snd x]

treeBuilder :: [HuffmanTree] -> HuffmanTree
treeBuilder [] = EmptyTree
treeBuilder [x] = x
treeBuilder (x:y:xs) = treeBuilder (sortTreeList (Node (getValue x + getValue y) x y : xs))

sortTreeList :: [HuffmanTree] -> [HuffmanTree]
sortTreeList [] = []
sortTreeList [x] = [x]
sortTreeList (x:xs) = sortTreeList [y | y <- xs, getValue y < getValue x] ++ [x] ++ sortTreeList [y | y <- xs, getValue y >= getValue x]

getValue :: HuffmanTree -> Int 
getValue EmptyTree = 0
getValue (LeafH x)= snd x
getValue (Node x _ _) = x

definitive :: [Char] -> HuffmanTree
definitive x = treeBuilder (sortTuple (freq x))


{-| Implement data type Graph -}

data Graph a = Graph [a] [(a, a)] deriving Eq

type Graph1 = [(Int, [Int])]

data GraphNode a = GraphNode a [GraphNode a]
type Graph2 a = [GraphNode a]

addVertex :: Graph a -> a -> Graph a
addVertex (Graph v e) x = Graph (v ++ [x]) e

addEdge :: Graph a -> (a, a) -> Graph a
addEdge (Graph v e) x = Graph v (e ++ [x])

paths :: Eq a => a -> a -> [(a,a)] -> [[a]] 
paths source sink edges 
    | source == sink = [[sink]]
    | otherwise = [
        source:path | edge<-edges, fst edge == source,
        path <- paths (snd edge) sink [e | e <- edges, e /= edge]]