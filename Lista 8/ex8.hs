-- 1.  Qual o tipo mais geral das seguintes funcoes:
-- head (a:x) = a <= [a] -> a
-- tail (a:x) = x <= [a] -> a
-- fst (t,u) = t <= (a,b) -> a
-- shift ((a,b),c) = (a,(b,c)) 


-- 2.  Definina a funcao concatena com um tipo polimorfico:
-- Hugs> concatena [[1,2,3], [2,3],[1]]
-- [1,2,3,2,3,1]

concatena :: [[a]] -> [a]
concatena []    = []
concatena (a:x) = a ++ concatena x

-- 3.  Defina a funcao inverte com um tipo polimorfico:
-- Hugs> inverte "abc"
-- "cba"
-- Hugs> inverte [1,2,3,4]
-- [4,3,2,1]

inverte :: [a] -> [a]
inverte []    = []
inverte (a:x) = inverte x ++ [a]

-- 4.  Defina  as  funcoes ultimo e inicio com  tipos  
-- polimorficos.  A  primeira retorna o ultimo elemento 
-- de uma lista e a segunda que  retorna toda uma lista 
-- menos o seu ultimo elemento

ultimo :: [a] -> a
ultimo []    = error "Lista vazia"
ultimo [a]   = a
ultimo (a:x) = ultimo x

inicio :: [a] -> [a]
inicio []    = []
inicio [a]   = []
inicio (a:x) = a : inicio x

-- 5.  Defina as funcoes take e drop com tipos polimorficos:
-- Hugs> take 2 [1,2,3,4]
-- [1,2]
-- Hugs> take 1 [True, False, False]
-- [True]
-- Hugs> drop 1 [True, False, False]
-- [False, False]
-- Hugs> drop 3 "cachorro"
-- "horro"

takee, dropp :: Int -> [a] -> [a]
-- takee
takee n [] = []
takee n (x:xs)
  | n < 0     = error "Número negativo"
  | n == 0    = []
  | otherwise = x : takee (n-1) xs
-- dropp
dropp n [] = []
dropp n (x:xs)
  | n < 0     = error "Número negativo"
  | n == 0    = (x:xs)
  | otherwise = dropp (n-1) xs