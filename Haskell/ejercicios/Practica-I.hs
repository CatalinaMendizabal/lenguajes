-- 1) Invertir una lista
invert :: [a] -> [a]
invert [] = []
invert (x:xs) = invert xs ++ [x]

-- 2) Sumar todos los elementos de una lista
sumElements :: (Num a) => [a] -> a
sumElements = sum

--sumElements = foldr (+) 0
{- sumElements [] = 0
sumElements (x:xs) = x + sumElements xs -}

-- 3) Obtener el mayor elemento de una lista
maxElement :: (Ord a) => [a] -> a
maxElement [] = error "Empty list"
maxElement [x] = x
maxElement (x:xs)
    | x > m = x
    | otherwise = m
    where m = maxElement xs

-- 4) Implementar la funcion de fibinacci
fibonacci :: (Integral a) => a -> a
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci i = fibonacci (i - 1) + fibonacci (i - 2)

-- 5) Permutar los valores de una lista
permutation :: [a] -> [a]
permutation [] = []
permutation [x] = [x]
permutation (x:y:xs) = [y] ++ [x] ++ permutation xs

-- 6) Ordernar una lista de tuplas a partir de su primer elemento
sortTuple :: (Ord a) => [(a, b)] -> [(a, b)]
sortTuple [] = []
sortTuple (x:xs) = sortTuple [ y | y <- xs, fst x > fst y] ++ [x] ++ sortTuple [ y | y <- xs, fst x <= fst y]

-- 7) Determinar si una lista es capicua
polindrome :: (Eq a) => [a] -> Bool
polindrome [] = True
polindrome [x] = True
polindrome (x:xs)
    | x == last xs = polindrome (init xs)
    | otherwise = False

-- 8) Insertar el valor x en la posicion i de una lista
insertElement :: [a] -> a -> Int -> [a]
insertElement [] _ _ = []
insertElement (x:xs) y i
    | i == 1 = [y] ++ [x] ++ xs
    | otherwise = x : insertElement xs y (i - 1)

-- 9) Calcular el tamano de una lista
listSize :: [a] -> Int
listSize xs = foldr (\ x -> (+) 1) 0 xs

-- 10) Determinar cuantas veces se repite el valor x en una lista
repeatElement :: (Eq a) => [a] -> a -> Int
repeatElement [] _ = 0
repeatElement (x:xs) y
    | x == y = 1 + z
    | otherwise = z
    where z = repeatElement xs y