-- 1.Defina  um  tipo ItemDeLocadora
-- Uma  locadora  pode  ter  Cds,  DVDs  e Videos. Coloque informacoes referentes 
-- a cada um dos itens na definicao do tipo. Ex: CDs: Artista, tıtulo do cd, etc

data ItemDeLocadora = CD Artista Titulo Ano | DVD Titulo Diretor Ano | Video Titulo Duracao
  deriving(Eq,Show)

type Artista = String
type Titulo = String
type Ano = Int
type Diretor = String
type Duracao = Int

cd :: ItemDeLocadora
cd = CD "Roberto Carlos" "Esse Cara Sou Eu" 1968

dvd :: ItemDeLocadora
dvd = DVD "Homem-Aranha" "Sam Raimi" 2002

video :: ItemDeLocadora
video = Video "Nyan Cat" 600

-- 2.Defina um tipo que represente um socio da locadora

data SocioLocadora = Socio Nome Numero [ItemDeLocadora]
  deriving(Eq,Show)

type Nome = String
type Numero = Int

socio :: SocioLocadora
socio = Socio "Juan" 23 []

-- 3.Defina um tipo que contenha todos os itens da locadora (ItensDisponiveis)

data ItensDisponiveis = Itens [ItemDeLocadora]
  deriving(Eq,Show)


itensDisponiveis :: ItensDisponiveis
itensDisponiveis = Itens [dvd,video]

-- 4.Defina  um  tipo  que  represente  um  item  alugado.  Um  item  alugado  deve
-- estar associado ao socio que alugou o item.

data ItemAlugado = Alugado SocioLocadora ItemDeLocadora
  deriving(Eq,Show)

itemAlugado :: ItemAlugado
itemAlugado = Alugado socio cd

-- 5.Defina um tipo que contenha todos os itens alugados

data TodosItensAlugados = TodosAlugados [ItemAlugado]
  deriving(Eq,Show)


todosItensAlugados :: TodosItensAlugados
todosItensAlugados = TodosAlugados [itemAlugado]

-- 6.Defina uma funcao que serve para alugar um item

alugarItem :: SocioLocadora -> ItemDeLocadora -> ItemAlugado
alugarItem s i = Alugado s i


-- 7.Defina uma funcao que retorne todos os itens alugados para uma pessoa


-- 8.Defina uma funcao com a qual um item pode ser devolvido para locadora


-- 9.Defina uma funcao que devolva todos os CDs que estao com uma pessoa