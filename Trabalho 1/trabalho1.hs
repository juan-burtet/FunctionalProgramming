-- que recebe como entrada uma string contendo um texto
-- em várias linhas e devolve o mesmo texto justificado
-- pela maior linha. Exemplo de texto:
-- justifica :: String -> String
-- justica texto =

-- Pega a String Completa (Inicio)
-- Tira Espaços Duplicados (removeEspacoDuplicado)
-- Separa por Linhas (separaLinhas)
-- Tamanho da Maior Linha (tamanhoMaiorLinha)
-- Tamanho da Maior Linha - Tamanho da Linha atual - Quantidade de Palavras da Linha Atual + 1 = Quantos Espaços faltam (quantosEpacos)
-- Fazer isso em todas as linhas



-- Quantidade de espaços entre palavras -> Espaços divido por palavras
-- Função que retorna a parte inteira da divisão
quantidadeInteira :: Int -> Int -> Int
quantidadeInteira n x = div n x
-- Função que retorna o resto da divisão
quantidadeResto :: Int -> Int -> Int
quantidadeResto n x = mod n x

-- Função que retorna quantos espaços são necessários na Linha
-- É Passado a linha e o tamanho da maior Linha
quantosEspacos :: String -> Int -> Int
quantosEspacos x tam = tam - length x - length (separaPalavras x) + 1

-- Função que separa as Strings em Linhas
separaLinhas :: String -> [String]
separaLinhas a = lines a

-- Função que separa as palavras por espaço
separaPalavras :: String -> [String]
separaPalavras a = words a

-- Função que retorna o tamanho da maior linha
tamanhoMaiorLinha :: [String] -> Int
tamanhoMaiorLinha []  = 0
tamanhoMaiorLinha [a] = length a
tamanhoMaiorLinha (x:xs)
  | length x > tamanhoMaiorLinha xs = length x
  | otherwise = tamanhoMaiorLinha xs

-- Função que insere Espaços nos conjuntos de Palavras da Linha
-- Conjunto de Palavras, qtInteira, qtResto
insereEspacosPalavras :: [String] -> Int -> Int -> String
insereEspacosPalavras [] a b  = []
insereEspacosPalavras [x] a b = x
insereEspacosPalavras (x:xs:xss) a b
  | b /= 0 = x ++ insereEspacosPalavras (insereEspacos xs (a+1):xss) a (b-1)
  | otherwise = x ++ insereEspacosPalavras (insereEspacos xs a :xss) a 0

-- Insere os Espacos nas Linhas
-- Conjunto de Linhas, qtInteira, qtResto
espacosNasLinhas :: [String] -> Int -> Int -> String
espacosNasLinhas [] a b     = ""
espacosNasLinhas (x:xs) a b = insereEspacosPalavras (words x) a b ++ "\n" ++ espacosNasLinhas xs a b

-- Função que insere N espaços na frente da String
insereEspacos :: String -> Int -> String
insereEspacos [] n = []
insereEspacos x  0 = x
insereEspacos x  n = ' ' : insereEspacos x (n-1)

-- Função que remove espaços duplicados
removeEspacoDuplicado :: String -> String
removeEspacoDuplicado []  = []
removeEspacoDuplicado [a] = [a]
removeEspacoDuplicado (x:xs:xss)
  | (x == ' ') && (xs == ' ') = removeEspacoDuplicado (xs:xss)
  | otherwise                 = x : removeEspacoDuplicado (xs:xss)
