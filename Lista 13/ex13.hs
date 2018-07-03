
lista :: [Int]
lista = [5,4,3,2,1]

exemplo :: [[Int]]
exemplo = [[1], [], [2,3,4], [9], [3,4], [6], [7,7,7]]

palavra :: String
palavra = "Roberto Carlos melhor cantor"

-- 1. Definir a funcao filter usando LC.

filterLC :: (a -> Bool) -> [a] -> [a]
filterLC f l = [x | x <- l, f x]

par :: Int -> Bool
par x = mod x 2 == 0

-- 2. Definir a funcao map usando LC.

mapLC :: (a->b) -> [a] -> [b]
mapLC f l = [f x | x <- l]

dobro :: Int -> Int 
dobro a = 2*a

-- 3. Definir a funcao
-- removeEspacos :: String -> String

removeEspacos :: String -> String
removeEspacos s = [x | x <- s, x /= ' ']

-- 4. Definir a funcao
-- sings :: [[a]] -> [a]
-- > sings [[1], [], [2,3,4], [9], [3,4], [6], [7,7,7]]
-- [1,9,6]

sings :: Eq a => [[a]] -> [a]
sings l = [x | (x:xs) <- l, xs == []]

-- 5. Definir a funcao
-- matches :: Int -> [Int] -> [Int]
-- que retorna as ocorrencias de um numero em uma lista
-- > matches 1 [1,2,44,1,33,2,7,1]
-- [1,1,1]
-- > matches 1 [3,2,8,6]
-- []

matches :: Int -> [Int] -> [Int]
matches n l = [x | x <- l, x == n]

-- 6. Usando a funcao matches definir a funcao elemento
-- > elemento 1 [1,2,44,1,33,2,7,1]
-- True
-- > matches 1 [3,2,8,6]
-- False

elemento :: Int -> [Int] -> Bool
elemento n l = (x /= [])
    where x = matches n l

-- 7. Definir a funcao
-- divisores :: Integer -> [ Integer]
-- > divisores 12
-- [1,2,3,4,6,12]

divisores :: Int -> [Int] 
divisores n = [x | x <- [1..n], mod n x == 0]

-- 8. Definir a funcao isPrime usando divisores

isPrime :: Int -> Bool
isPrime n = (length x == 2)
  where x = divisores n

-- 9. Dada a funcao quickSort

quickSort :: Ord a => [a] -> [a]
quickSort []    = []
quickSort (a:l) = quickSort (menores a l) ++ [a] ++ quickSort (maiores a l)

-- Definir as funcoes menores e maiores usando LC

menores :: Ord a => a -> [a] -> [a]
menores a l = [x | x <- l, x < a]

maiores :: Ord a => a -> [a] -> [a]
maiores a l = [x | x <- l, x >= a]