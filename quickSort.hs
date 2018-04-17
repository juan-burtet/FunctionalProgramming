-- Quicksort

quickSort :: [Int] -> [Int]
quickSort []     = []
quickSort (x:xs) = quickSort (menores x xs) ++ [x] ++ quickSort (maiores x xs)

menores :: Int -> [Int] -> [Int] 
menores a [] = []
menores a (x:xs)
  | x < a     = x : menores a xs 
  | otherwise = menores a xs


maiores :: Int -> [Int] -> [Int]
maiores a [] = []
maiores a (x:xs)
  | x >= a    = x : maiores a xs 
  | otherwise = maiores a xs