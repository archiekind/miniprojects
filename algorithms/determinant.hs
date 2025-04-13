determinant :: Int -> [[Int]] -> Int
determinant _ [[x]] = x
determinant n xs = finish 0 xs cofactors
  where
        finish :: Int -> [[Int]] -> [Int] -> Int
        finish _ [] [] = 0
        finish n (x:xs) (c:cofactors) = head x * c * ((-1) ^ n) + finish (n + 1) xs cofactors
        cofactors = reverse $ map (determinant (n - 1)) (cofactor n xs)

cofactor :: Int -> [[Int]] -> [[[Int]]]
cofactor 0 xs = [minor 0 xs]
cofactor n xs = minor n xs : cofactor (n - 1) xs

-- need to remove 1st row and nth column
minor :: Int -> [[Int]] -> [[Int]]
minor _ [] = []
minor 0 (x:xs) = minor (-1) xs
minor n (x:xs) = tail x : minor (n - 1) xs