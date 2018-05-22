-- que recebe como entrada uma string contendo um texto 
-- em várias linhas e devolve o mesmo texto justificado 
-- pela maior linha. Exemplo de texto:
--justifica :: String -> String
--justica texto = tamanhoMaiorLinha ()

-- Função que separa as Strings em Linhas
separaLinhas :: String -> [String]
separaLinhas a = lines a

-- Função que separa as palavras por espaço
separaPalavras :: String -> [String]
separaPalavras a = words a

-- Função que separa as palavras em um conjunto de todas as linhas
separaPalavrasLinhas :: [String] -> [[String]]
separaPalavrasLinhas [] = []
separaPalavrasLinhas (x:xs) = separaPalavras x : separaPalavrasLinhas xs


-- Função que retorna o tamanho da maior linha
tamanhoMaiorLinha :: [String] -> Int
tamanhoMaiorLinha []  = 0
tamanhoMaiorLinha [a] = length a
tamanhoMaiorLinha (x:xs)
  | length x > tamanhoMaiorLinha xs = length x
  | otherwise = tamanhoMaiorLinha xs

-- Função que inseres N espaços no inicio da String
insereEspacos :: Int -> String -> String
insereEspacos 0 s  = s
insereEspacos n [] = []
insereEspacos n s 
  | n < 0  = error "Número Negativo"
  | n == 0 = s
  | otherwise = ' ' : insereEspacos (n-1) s

-- Função que Insere os N espaços entre o Conjunto de Palavras
insereEspacosLinha :: Int -> [String] -> String
insereEspacosLinha n []  = []
insereEspacosLinha n [a] = a
insereEspacosLinha n (x:xs:xss)
  | n <  0 = error "Número Negativo"
  | n == 0 = x ++ insereEspacosLinha n (xs:xss)
  | otherwise = x ++ insereEspacosLinha n ((insereEspacos n xs) : xss)

-- Função que remove espaços duplicados
removeEspacoDuplicado :: String -> String
removeEspacoDuplicado []  = []
removeEspacoDuplicado [a] = [a]
removeEspacoDuplicado (x:xs:xss)
  | (x == ' ') && (xs == ' ') = removeEspacoDuplicado (xs:xss)
  | otherwise                 = x : removeEspacoDuplicado (xs:xss)


-- Função que conta a quantidade de palavras
contaPalavras :: [String] -> Int
contaPalavras [] = 0
contaPalavras (x:xs) = 1 + contaPalavras xs

