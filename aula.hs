mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f []    = []
mapInt f (a:x) = f a : mapInt f x

times2, times3 :: Int -> Int
times2 n = 2*n
times3 n = 3*n


-- Tipos Algébricos

-- Tipo = Construtores
data Temperatura = Frio | Calor
  deriving(Eq,Show)

data Estacao = Verao | Outono | Inverno | Primavera
  deriving(Eq,Show)

tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo x     = Frio

data Funcionario = Pessoa Nome Idade
  deriving(Eq,Show)

type Nome = String
type Idade = Int

andre :: Funcionario
andre = Pessoa "Andre Du Bois" 28

pegaNome :: Funcionario -> Nome
pegaNome (Pessoa n i) = n

pegaIdade :: Funcionario -> Idade
pegaIdade (Pessoa n i) = i

data Forma = Circulo Float | Retangulo Float Float
  deriving(Eq,Show)

roberto :: Forma
roberto = Circulo 2.0

carlos :: Forma
carlos = Retangulo 2.0 3.0


redondo :: Forma -> Bool
redondo (Circulo x) = True
redondo (Retangulo x y) = False

area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo b a) = b * a
