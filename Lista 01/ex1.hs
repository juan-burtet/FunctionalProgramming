-- 1. Escreva a função osQuatroSaoIguais que possui tipo
--		Int -> Int -> Int -> Int -> Bool
-- que retorna True se seus quatro argumentos são iguais

osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais a b c d
  | (a == b) && (b == c) && (c == d) = True
  | otherwise                        = False

-- 2. Defina a função quantosSaoIguais :: Int -> Int -> Int -> Int que
-- conta quantos argumentos iguais a função recebeu:

quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais a b c
  | (a == b) && (b == c)             = 3
  | (a /= b) && (b /= c) && (a /= c) = 0
  | otherwise                        = 2

-- 3. Defina a função
--		todosDiferentes :: Int -> Int -> Int -> Bool
-- que retorna True se todos os seus argumentos são diferentes. Obs: m /= n
-- retorna True se m e n são diferentes

todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c
  | quantosSaoIguais a b c == 0 = True
  | otherwise                   = False

-- 4. Defina um conjunto de testes para a função todosDiferentes

-- 5. O que está errado com a seguinte definição de todosDiferentes:
-- todosDiferentes n m p = ( ( n/=m ) && ( m/=p ) )
-- O conjunto de testes que você definiu na questão anterior funciona com
-- esta definição? Não, pois caso n fosse igual a p, diria que todos são diferentes

-- 6. Defina a funcao
-- 		todosIguais :: Int -> Int -> Int -> Bool

todosIguais :: Int -> Int -> Int -> Bool
todosIguais a b c
  | quantosSaoIguais a b c == 3 = True
  | otherwise                   = False

