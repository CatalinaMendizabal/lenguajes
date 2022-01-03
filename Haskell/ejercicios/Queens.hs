module Queens where

-- array index means column, number in array means row
-- [1, 3, 5, 6] means positions are (1,1) (3,2) (5,3) (6,4)


queens :: Int -> [[Int]]
queens n = queens' n
        where 
            queens' 0 = [[]]
            queens' k = [q:qs | qs <- queens' (k-1), q <- [1..n], valid q qs]
            valid try qs = not (sameRow try qs || sameDiag try qs)
            sameRow = elem
            sameDiag try qs = any (\(colDist, q) -> abs (try - q) == colDist) $ zip [1..] qs