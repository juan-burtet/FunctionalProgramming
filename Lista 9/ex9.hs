-- 1. Defina a funcao concat usando fold
-- Hugs> concat [[1,2,3],[1],[3,3]]
-- [1,2,3,1,3,3]
concate :: [[a]] -> [a]
concate a = foldr (++) [] a

-- 2. Defina a funcao and usando o fold
-- Hugs> and [True, False,True, True]
-- False
andf :: [Bool] -> Bool
andf a = foldr (&&) True a

-- 3. Use filter e a funcao par para definir uma 
-- funcao que retorne os numeros pares de uma lista
-- Hugs> pares [1,2,4,5]
-- [2,4]
par :: Int -> Bool
par a = mod a 2 == 0

pares :: [Int] -> [Int]
pares a = filter par a

-- 4. Implemente a funcao que inverte uma lista usando
-- foldr

inverteLista :: [a] -> [a]
inverteLista a = foldr (\a b -> b ++ [a]) [] a

-- 5. Defina uma funcao que calcule a soma dos quadrados dos 
-- numeros de uma lista usando map e fold

somaQuadrados :: [Int] -> Int
somaQuadrados a = foldr (+) 0 (map (^2) a)

-- 6. Defina uma funcao que calcule a soma dos quadrados dos 
-- numeros positivos de uma lista.

somaQuadradosPositivos :: [Int] -> Int
somaQuadradosPositivos a = foldr (+) 0 (map (^2) (filter (>0) a))

-- 7. Implemente a funcao que calcula o numero de elementos
-- de uma lista usando apenas fold e map

numeroElementos :: [a] -> Int
numeroElementos a = foldr (+) 0 (map (\x -> 1) a)

-- 8. Defina a funcao que some os elementos de uma lista
-- de listas (usando map e fold)
-- Hugs> somaListas [[1,2,3,4],[4], [3,2],[3]]
-- 22

somaListas :: [[Int]] -> Int
somaListas a = foldr (+) 0 (map (foldr (+) 0) a)

-- 9. Defina uma funcao que calcule o tamanho total dos elementos 
-- de uma lista de listas (usando map e fold)
-- Hugs> somaTamanhoListas [[1,2,3,4],[4], [3,2],[3]]
-- 8

