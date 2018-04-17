-- Ordenação
-- Insertion Sort

ins :: Int -> [Int] -> [Int]
ins a [] = [a]
ins a (x:xs)
  | a >  x = x : ins a xs 
  | a <= x = a : x : xs

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = ins x (insertionSort xs)

-- Elimina repetições
insertionSort_1 :: [Int] -> [Int]
insertionSort_1 []     = []
insertionSort_1 (x:xs) = ins_1 x (insertionSort_1 xs)

ins_1 :: Int -> [Int] -> [Int]
ins_1 a [] = [a]
ins_1 a (x:xs)
  | a == x = x:xs
  | a >  x = x : ins_1 a xs 
  | a < x  = a : x : xs

-- Ordena de forma decrescente
insertionSort_2 :: [Int] -> [Int]
insertionSort_2 []     = []
insertionSort_2 (x:xs) = ins_2 x (insertionSort_2 xs)

ins_2 :: Int -> [Int] -> [Int]
ins_2 a [] = [a]
ins_2 a (x:xs)
  | a > x     = a : x : xs
  | otherwise = x : ins_2 a xs


minEmax :: [Int] -> (Int,Int)
minEmax [] = (-1,-1)
minEmax a  = (minimo a, maximo a)

pegaMinimo :: [Int] -> Int
pegaMinimo []     = -1
pegaMinimo (x:xs) = x

pegaMaximo :: [Int] -> Int
pegaMaximo []     = -1
pegaMaximo (x:xs) = x

minimo :: [Int] -> Int
minimo [] = -1
minimo x  = pegaMinimo (take 1 (insertionSort x))

maximo :: [Int] -> Int
maximo [] = -1
maximo x  = pegaMaximo (take 1 (insertionSort_2 x))