{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parcial where
import Distribution.Simple.Command (OptDescr(BoolOpt))

{- 1- Dado el tipo de dato "data Bit = ZERO | ONE", definir la suma binaria. 
    [ONE, ZERO] [ONE, ONE] = [ONE ZERO ONE]
-}

data Bit = ZERO | ONE

binarySum :: [Bit] -> [Bit] -> [Bit]
binarySum [] [] = []
binarySum [] l = l
binarySum l [] = l
binarySum a b
    | carry == ONE = binarySum (binarySum (init a) (init b)) [carry] ++ [res]
    | otherwise = binarySum (init a) (init b) ++ [res]
    where (carry, res) = bitAdder (last a) (last b)

bitAdder :: Bit -> Bit -> (Bit, Bit)
bitAdder ZERO ZERO = (ZERO, ZERO)
bitAdder ZERO ONE = (ZERO, ONE)
bitAdder ONE ZERO = (ZERO, ONE)
bitAdder ONE ONE = (ONE, ZERO)

instance Eq Bit where
    ONE == ONE = True
    ZERO == ZERO = True
    _ == _ = False

{- 2 Definir el tipo de dato TREE (árbol binario con valores solo en las hojas) e implementar la clase Eq -}

data Tree a = Empty | Branch (Tree a) (Tree a) | Leaf a deriving (Show)

instance (Eq a) => Eq (Tree a) where
    Empty == Empty = True
    Leaf x == Leaf y = x == y
    Branch l r == Branch l' r' = l == l' && r == r'
    _ == _ = False

{-| (3) A partir del los tipos de dato definido en los ejercicios 1 y 2, definir una función donde se le pase un Tree y una 
lista de Bit, retornar un String que sea el recorrer el Tree según la lista de Bit, donde ZERO es izquierda y ONE es derecha. -}

huffmanDecode :: Tree Char -> [Bit] -> [Char]
huffmanDecode t [] = []
huffmanDecode t c = x : huffmanDecode t xs
    where (x, xs) = decode t c

decode :: Tree Char -> [Bit] -> (Char, [Bit])
decode (Leaf v) [] = (v, [])
decode _ [] = error "invalid code"
decode Empty _ = error "invalid code"
decode (Leaf v) c = (v, c)
decode (Branch l r) (x:xs)
    | x == ZERO = decode l xs
    | otherwise = decode r xs

{- Ordenar una lista de elementos comparables -}

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y < x] ++ [x] ++ sort [z | z <- xs, z >= x]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

{- Definir un tipo de dato Tree e implementar una funcion que valide 
    si existe un valor dentro del arbol -}

-- mismo data Tree a = Empty | Branch (Tree a) (Tree a) | Leaf a deriving (Show)

hasElement :: (Eq a) => Tree a -> a -> Bool
hasElement Empty a = False
hasElement (Leaf l) a = l == a
hasElement (Branch l r) a = hasElement l a || hasElement r a

{- Implementar una funcion que chequee si existen ciclos 
    Por Wawey ???
-}
type Graph = [(Int, [Int])]

hasCycles :: Graph -> Bool
hasCycles graph = any ((`hasCycles''` graph) . fst) graph
-- hasCycles graph = any (\x -> hasCycles'' x graph) $ map fst graph

hasCycles'' :: Int -> Graph -> Bool
hasCycles'' start graph = hasCycles' graph start [] (getNeighbors start graph)

hasCycles' :: Graph -> Int -> [Int] -> [Int] -> Bool
hasCycles' _ _ _ [] = False
hasCycles' graph current visited neighbors = current `elem` visited || any (\x -> hasCycles' graph x (current:visited) (getNeighbors x graph)) neighbors

getNeighbors :: Int -> Graph -> [Int]
getNeighbors a [] = error $ "Node " ++ show a ++ " does not exist"
getNeighbors a (n:ns) = if a == fst n then snd n else getNeighbors a ns

{- Drive
xCicle :: Graph -> [Int] -> [Int] -> Bool
xCicle g _ [] = False
xCicle g v (x:xs) = 
	| contains v x = True -- si los vecinos contienen al primer elemento, True
	| xCicle g (v ++ [x]) snd(get g x) = True --
	| otherwise = xCicle g v xs

get :: Graph -> Int -> (Int, [Int])
get []_ = error "Not found"
get (x:xs) i = 
	|fst x == i = x
	|otherwise = get xs i

isCicle :: Graph -> Bool
isCicle [] = False
isCicle x:xs = foldl(\yz -> y||z) False (map(\w -> xCicle x:xs fst w snd w) x:xs) -- xCicle x:xs [fst x] snd x
	--|xCicle x:xs [fst x] snd x = True
-}

{-Definir Tree y una funcion que retorne el elemento mas grande del arbol -}

data Tree2 b = Empty2 | Node b (Tree2 b) (Tree2 b) | Leaf2 b deriving (Show)

searchMaxElement :: (Ord b) => Tree2 b -> b
searchMaxElement Empty2 = error "Empty tree"
searchMaxElement (Leaf2 l) = l
searchMaxElement (Node a l r)
    | a > i && a > j = a
    | otherwise = if j >= i then j else i
    where i = searchMaxElement l
          j = searchMaxElement r


{- Dada la tabla de codificacion [(caracter, codigo)] y una cadena de 
    [caracter], retornar una nueva cadena que contenga su codificacion [codigo]
-}

cadena:: (Eq a)=>[(a,b)]->[a] ->[b]
cadena _ [] = []
cadena (x:xs) [a] = [getCode (x:xs) a]
cadena (b:bs) (a:as) = map (getCode (b:bs)) (a:as) -- importe warning


getCode:: (Eq a) => [(a,b)] -> a -> b
getCode [] _ = error "lo que quieras"
getCode [x] c
    |fst x == c = snd x
    |otherwise = error "lo que quieras"
getCode (a:as) c
    |fst a == c = snd a
    |otherwise = getCode as c

