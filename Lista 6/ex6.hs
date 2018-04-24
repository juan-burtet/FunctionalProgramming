-- 1. Defina a função
-- somaTripla :: [(Int,Int,Int)] -> Int

somaTripla :: [(Int,Int,Int)] -> Int
somaTripla []           = 0
somaTripla ((a,b,c):xs) = a + b + c + somaTripla xs

-- 2. Definina a seguinte versão de somaTuplas
-- somaTupla :: [((Int,Int),(Int,Int))] -> Int

somaTupla :: [((Int,Int),(Int,Int))] -> Int
somaTupla []               = 0
somaTupla (((a,b),(c,d)):xs) = a + b + c + d + somaTupla xs

-- 3. Defina a função
-- zipp :: [Int] -> [Int] -> [(Int,Int)]
-- que transforma duas listas de inteiros em uma lista de tuplas:
-- Prelude> zipp [1,2,3] [1,2,3,4,5]
-- [(1,1),(2,2),(3,3)]

zipp :: [Int] -> [Int] -> [(Int,Int)]
zipp [] []         = []
zipp [] a          = []
zipp a  []         = []
zipp (x:xs) (y:ys) = (x,y) : zipp xs ys

-- 4. Defina a função zip3 que faz o zipp de três listas

zipp3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
zipp3 [] a b = []
zipp3 a [] b = []
zipp3 a b [] = []
zipp3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zipp3 xs ys zs

-- 5. Defina a função unZipp
-- unZipp :: [(Int,Int)] -> ([Int], [Int])
-- essa função pode ser mais facilmente definida usando as funções:
-- unzipEsq, unzipDir ::
-- [(Int,Int)] -> [Int]
-- onde:
-- unzipEsq [(2,3), (4,9), (5,6)] = [2,4,5]
-- unzipDir [(2,3), (4,9), (5,6)] = [3,9,6]

unZipp :: [(Int, Int)] -> ([Int], [Int])
unZipp [] = error "Lista vazia"
unZipp a  = (unzipEsq a, unzipDir a)
  where
    unzipEsq :: [(Int,Int)] -> [Int]
    unzipEsq []         = []
    unzipEsq ((a,b):xs) = a : unzipEsq xs
    unzipDir :: [(Int,Int)] -> [Int]
    unzipDir []         = []
    unzipDir ((a,b):xs) = b : unzipDir xs