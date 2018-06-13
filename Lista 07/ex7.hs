-- Função exemplo
mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f []    = []
mapInt f (a:x) = f a : mapInt f x


--1. Defina novamente a funcão total, que calcula o total de vendas de varias
--semanas. Agora a funcao total deve receber como argumento a funcao que
--determina as vendas de uma semana:
--total :: (Int -> Int) -> Int -> Int
total :: (Int -> Int) -> Int -> Int
total f 0 = f 0
total f n = (f n) + total f (n-1)

vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 20
vendas 2 = 30
vendas 3 = 40
vendas 4 = 50
vendas n = n*10 + 10

--2. Definina a funcao foldInt
--foldInt :: (Int -> Int -> Int) -> [Int] -> Int
--soma :: Int -> Int -> Int
--soma x y = x + y
-- > foldInt soma [1,2,3]
--6
soma :: Int -> Int -> Int
soma x y = x + y

foldInt :: (Int -> Int -> Int) -> [Int] -> Int
foldInt f []  = error "lista vazia!"
foldInt f [x] = x
foldInt f (x:y:xs) = f (f x y) (foldInt f xs)

--3. Testar a função foldInd com mais dois exemplos

sub :: Int -> Int -> Int
sub x y = x - y

mult :: Int -> Int -> Int
mult x y = x*y

--4. Defina a funcao
-- filterString :: (Char -> Bool) -> [Char] -> [Char]
-- naoEspaco :: Char -> Bool
-- naoEspaco x = x /= ’ ’
-- > filterString naoEspaco "Andre Du Bois "
--"AndreDuBois"

filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString f []     = []
filterString f (x:xs) 
  | f x == True = filterString f xs
  | otherwise   = x : filterString f xs

naoEspaco :: Char -> Bool
naoEspaco ' ' = True
naoEspaco x   = False


--5. Testar filterString com mais dois exemplos

naoVogal :: Char -> Bool
naoVogal 'a' = True
naoVogal 'e' = True
naoVogal 'i' = True
naoVogal 'o' = True
naoVogal 'u' = True
naoVogal 'A' = True
naoVogal 'E' = True
naoVogal 'I' = True
naoVogal 'O' = True
naoVogal 'U' = True
naoVogal x   = False

apenasVogais :: Char -> Bool
apenasVogais x
  | naoVogal x == True = False
  | otherwise              = True


--6. Usando as funcoes de alta ordem definidas nos exercıcios anteriores 
--(incluindo o mapInt), defina uma funcao que devolva a soma do 
--quadrado dos numeros em uma lista l

quadrado :: Int -> Int
quadrado x = x*x

somaQuadrados :: [Int] -> Int
somaQuadrados l = foldInt soma (mapInt quadrado l)

--6.1 Calcular o tamanho de uma lista usando MapInt e FoldInt

conta :: Int -> Int
conta x = 1

sizeLista :: [Int] -> Int
sizeLista l = foldInt soma (mapInt conta l)

--7. Defina uma funcao que testa se os valores gerados por uma funcao f
--aplicada a valores de 0 ate n sao maiores que zero

maioresZero :: (Int -> Int) -> Int -> Bool
maioresZero f n
  | n < 0 == error "N menor que zero"
  | f 0 == True = True
  | f n == True = f (n-1)
  | f n == False = False


--8. Defina uma funcao duasVezes que recebe uma funcao de Inteiros
--para Inteiros, um Inteiro e aplique a funcao duas vezes ao argumento
--Ex: Hugs> duasVezes times2 2
-- 8

duasVezes :: (Int -> Int) -> Int -> Int
duasVezes f n = f (f n)

times2 :: Int -> Int
times2 x = 2*x

--9. De o tipo e defina a funcao inter, onde
-- inter 3 f x =  f(f (f x))

inter :: Int -> (Int -> Int) -> Int -> Int
inter 0 f x = error "Digitou 0"
inter 1 f x = f x
inter n f x = f (inter (n-1) f x)