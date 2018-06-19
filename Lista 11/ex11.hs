data Arvore a = Folha a | No a (Arvore a) (Arvore a)
  deriving(Eq,Show)


arv1 :: Arvore Int
arv1 = No 3 (No 2 (Folha 4) (Folha 5)) (Folha 1)

-- 1. Defina uma funcao que multiplique por 2 os inteiros em uma arvore.
doubleArvore :: Arvore Int -> Arvore Int
doubleArvore (Folha x) = Folha (2*x)
doubleArvore (No x a1 a2) = No (2*x) (doubleArvore a1) (doubleArvore a2)

-- 2. Defina uma funcao que conte quantos elementos existem em uma arvore

countArvore :: Arvore a -> Int
countArvore (Folha x) = 1
countArvore (No x a1 a2) = 1 + countArvore a1 + countArvore a2

-- 3. Defina uma funcao que ache o maior elemento de uma arvore

maxArvore :: (Ord a) => Arvore a -> a
maxArvore (Folha x) = x
maxArvore (No x a1 a2) = max x (max (maxArvore a1) (maxArvore a2))

-- 4. Defina a funcao que diz se um inteiro ocorre dentro de uma arvore

isIntArvore :: Arvore Int -> Int -> Bool
isIntArvore (Folha x) n = (x == n)
isIntArvore (No x a1 a2) n = (x == n) || (isIntArvore a1 n) || (isIntArvore a2 n) 

-- 5. Defina uma funcao que diz quantas vezes um inteiro ocorre dentro de uma
-- arvore

countIntArvore :: Arvore Int -> Int -> Int
countIntArvore (Folha x) n
  | x == n = 1
  | otherwise = 0
countIntArvore (No x a1 a2) n
  | x == n = 1 + soma
  | otherwise = 0 + soma
    where
      soma = (countIntArvore a1 n) + (countIntArvore a2 n)

-- 6. Uma arvore refletida e uma arvore com seus ramos esquerdos e direitos
-- trocados. Defina uma funcao refleteArvore

refleteArvore :: Arvore a -> Arvore a
refleteArvore (Folha x) = Folha x
refleteArvore (No x a1 a2) = No x (refleteArvore a2) (refleteArvore a1)

-- 7.  Defina uma funcao que calcule a altura de uma arvore

alturaArvore :: Arvore a -> Int
alturaArvore arv = (alturaArvoreAux arv 0) 

alturaArvoreAux :: Arvore a -> Int -> Int
alturaArvoreAux (Folha x) n = n + 1
alturaArvoreAux (No x a1 a2) n = max (alturaArvoreAux a1 (n+1)) (alturaArvoreAux a2 (n+1))

-- 8.  Defina uma funcao que transfore uma arvore em uma lista

-- 9.  Defina a funcao mapTree que aplica uma funcao a todos os inteiros de todos
-- os nos de uma arvore.
-- mapTree :: (a -> b) -> Arvore a -> Arvore b