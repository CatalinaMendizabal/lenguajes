module TP2 where

isPrime :: Int -> Bool
isPrime n = isPrime' n 2

isPrime' :: Int -> Int -> Bool
isPrime' n current
        | current <= (floor $ sqrt $ fromIntegral n) = (n `mod` current) /= 0 && isPrime' n (current + 1)
        | otherwise = True

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose m = map head m : transpose (map tail m)

data Contact = Contact{name::String, phone::Int, email::String}
type Agenda = [Contact]

addContact :: Contact -> Agenda -> Agenda
addContact c a = c : a

findByName :: String -> Agenda -> Maybe Contact
findByName n [] = Nothing
findByName n (c:cs)
        | n == name c = Just c
        | otherwise = findByName n cs

findByEmail :: String -> Agenda -> Maybe Contact
findByEmail e [] = Nothing
findByEmail e (c:cs)
        | e == email c = Just c
        | otherwise = findByEmail e cs