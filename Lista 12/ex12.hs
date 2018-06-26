-- 1. Como seria a definicao do tipo lista usando um tipo algebrico recursivo
-- polimorfico?

data Lista a = Cons a (Lista a) | Vazio
  deriving(Eq,Show)

-- 2. Definir as funcoes map, foldr1, filter, (++), reverse, concat, take e drop
-- para o novo tipo de listas.

newMap :: (a -> b) -> Lista a -> Lista b
newMap f Vazio       = Vazio
newMap f (Cons a xs) = Cons (f a) (newMap f xs) 

newFilter :: (a -> Bool) -> Lista a -> Lista a
newFilter f Vazio       = Vazio
newFilter f (Cons a xs) 
  | f a       = Cons a (newFilter f xs)
  | otherwise = newFilter f xs

maisMais :: Lista a -> Lista a -> Lista a
maisMais Vazio a       = a
maisMais a Vazio       = a
maisMais (Cons a xs) b = (Cons a (maisMais xs b))

newReverse :: Lista a -> Lista a
newReverse Vazio       = Vazio
newReverse (Cons a xs) = maisMais (newReverse xs) (Cons a Vazio)

newConcat :: Lista (Lista a) -> Lista a
newConcat Vazio       = Vazio
newConcat (Cons a xs) = maisMais a (newConcat xs)

-- 3. Como poderiamos definir o tipo tupla de 2 elementos usando um tipo
-- algebrico polimorfico?

-- 4. Definir as funcoes zip, unzip e splitAt usando o tipo os novos tipos para
-- listas e tuplas.


----
ex1 :: Lista Int
ex1 = Cons 1 (Cons 2 (Cons 3 (Cons 4 Vazio)))

ex2 :: Lista Int
ex2 = Cons 5 (Cons 6 (Cons 7 (Cons 8 Vazio)))

ex3 :: Lista (Lista Int)
ex3 = Cons (Cons 1 (Cons 2 Vazio)) (Cons (Cons 3 (Cons 4 Vazio)) (Cons Vazio Vazio))


func :: Int -> Bool
func x
  | mod x 2 == 0 = True
  | otherwise = False