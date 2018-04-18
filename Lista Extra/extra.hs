-- (1) Implementar a função retornaUltimo
-- > retornaUltimo [1,2,3]
-- 3

retornaUltimo :: [a] -> a
retornaUltimo []     = error "Lista vazia"
retornaUltimo [a]    = a
retornaUltimo (x:xs) = retornaUltimo xs


--(2) Implementar a função pegaPosicao
-- > pegaPosicao 3 [10,2,3,4,5]
--3

pegaPosicao :: Int -> [a] -> a
pegaPosicao a [] = error "Lista vazia"
pegaPosicao a (x:xs)
  | a <= 0    = error "Posição Invalida"
  | a == 1    = x
  | otherwise = pegaPosicao (a - 1) xs

--(3) Implementar a função pega:
-- > pega 3 [1,2,3,4,5]
--[1,2,3]

pega :: Int -> [a] -> [a]
pega a [] = []
pega a (x:xs)
  | a <= 0    = []
  | otherwise = x : pega (a-1) xs


--(4) Implementar a função retira:
-- > retira 3 [1,2,3,4,5]
--[4,5]

retira :: Int -> [a] -> [a]
retira a [] = []
retira a (x:xs)
  | a < 0     = error "Número negativo!" 
  | a == 0    = (x:xs)
  | otherwise = retira (a-1) xs

--(5) Implementar uma função que cacula a média dos elementos de uma lista

mediaLista :: [Int] -> Float
mediaLista [] = 0
mediaLista a  = fromIntegral (soma a) / fromIntegral (quantidade a)
 where
  soma :: [Int] -> Int
  soma []      = 0
  soma (x:xs)  = x + soma xs
  quantidade :: [Int] -> Int
  quantidade []     = 0
  quantidade (x:xs) = 1 + quantidade xs

--(6) Implementar a função pegaMaiores
-- > pegaMaiores 3 [10,2,3,4,5]
--[10,4,5]

pegaMaiores :: Int -> [Int] -> [Int]
pegaMaiores a [] = []
pegaMaiores a (x:xs)
  | x > a     = x : pegaMaiores a xs
  | otherwise = pegaMaiores a xs


--(7) Implementar a função contaMaiores
-- > contaMaiores 3 [10,2,3,4,5]
--3

contaMaiores :: Int -> [Int] -> Int
contaMaiores a [] = 0
contaMaiores a (x:xs)
  | x > a     = 1 + contaMaiores a xs
  | otherwise = contaMaiores a xs


--(8) Implementar a função contatena
-- > concatena "abc" "123"
--"abc123"
-- > concatena [1,2] [3,4,5]
--[1,2,3,4,5]

concatena :: [a] -> [a] -> [a]
concatena a []     = a
concatena [] a     = a
concatena (x:xs) a = x : concatena xs a

--(9) Implementar a função intercala
-- > intercala [1,2,3,4] [10,20,30]
--[1,10,2,20,3,30,4]

intercala :: [a] -> [a] -> [a]
intercala [] a = a
intercala b [] = b
intercala (x:xs) (y:ys) = x : y : intercala xs ys

--(10) Implementar a função compress que elimina elementos consecutivos
-- > compress "aaaabccaadeeee"
--"abcade"

compress :: [a] -> [a]
compress []  = []
compress [x] = [x]
compress (x:y:xs)
  | x == y      = compress (y:xs)
  | otherwise   = x : compress (y:xs)


--(11) Implementar a função pack que coloca elementos consecutivos em sublistas
-- > pack [’a’, ’a’, ’a’, ’a’, ’b’, ’c’, ’c’, ’a’,
--’a’, ’d’, ’e’, ’e’, ’e’, ’e’]
--["aaaa","b","cc","aa","d","eeee"]


--(12) Implementar a função encode:
-- > encode "aaaabccaadeeee"
--[(4,’a’),(1,’b’),(2,’c’),(2,’a’),(1,’d’),(4,’e’)]


--(13) Implementar a função dupli, que duplica os elementos de uma lista:
-- > dupli [1, 2, 3]
--[1,1,2,2,3,3]


--(14) Implementar a função repli, que replica os elementos de uma lista:
-- > repli 3 "abc"
--"aaabbbccc"

--(15)Implementar a função drobEvery, que retira cada nth elemento de uma lista
-- *Main> dropEvery 3 "abcdefghik"
--"abdeghk"


--(16)Implementar a função split
-- *Main> split 3 "abcdefghik"
--("abc", "defghik")

split :: Int -> [a] -> ([a],[a])
split a [] = ([], [])
split a (x:xs) = ([],[])


--(17)Implementar a função slice, que devolve um pedaço interno de uma lista:
-- *Main> slice 3 7 [’a’,’b’,’c’,’d’,’e’,’f’,’g’,’h’,’i’,’k’]
-- "cdefg"


--(18)Implementar a função rotate
-- *Main> rotate [’a’,’b’,’c’,’d’,’e’,’f’,’g’,’h’] 3
-- "defghabc"
-- *Main> rotate [’a’,’b’,’c’,’d’,’e’,’f’,’g’,’h’] (-2)
-- "ghabcdef"

--(19) Implementar a função removeAt
-- *Main> removeAt 2 "abcd"
--(’b’,"acd")