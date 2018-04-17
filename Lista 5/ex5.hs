-- 1. Defina a função "membro" que retorna um booleando que diz se o inteiro esta na lista 
membro :: [Int] -> Int -> Bool
membro [] a = False
membro (x:xs) a
  | x == a    = True
  | otherwise = membro xs a


-- 2. Implemente a função membroNum que conta o número de vezes que o inteiro aparece na lista
membroNum :: [Int] -> Int -> Int
membroNum [] a = 0
membroNum (x:xs) a
  | x == a    = 1 + membroNum xs a
  | otherwise = 0 + membroNum xs a

-- 3. Defina a função membro usando a função membroNum
{-
membro :: [Int] -> Int -> Bool
membro a b 
  | (membroNum a b) == 0 = False
  | otherwise          = True
-}

-- 4. Implemente a função unico que retorna uma lista com os numeros que aparecem apenas uma vez na lista argumento.
unico :: [Int] -> [Int]
unico [] = []
unico x  = unicoAux x (insertionSort_1 x)



unicoAux :: [Int] -> [Int] -> [Int]
unicoAux a (x:xs)
  | membroNum a (x:xs) == 1 = a : unicoAux a xs
  | otherwise               = unicoAux a xs


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