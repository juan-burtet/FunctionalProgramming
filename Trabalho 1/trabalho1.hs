-- que recebe como entrada uma string contendo um texto
-- em várias linhas e devolve o mesmo texto justificado
-- pela maior linha. Exemplo de texto:
justifica :: String -> String
justifica a = espacosNasLinhas texto (tamanhoMaiorLinha texto)
  where
    texto = separaLinhas (consertaTexto a)

-- Função que retorna a quantidade inteira de uma divisão
-- n -> x -> n/x
quantidadeInteira :: Int -> Int -> Int
quantidadeInteira n x = div n x

-- Função que retorna o resto de uma divisão
-- n -> x -> n%x
quantidadeResto :: Int -> Int -> Int
quantidadeResto n x = mod n x

-- Função que retorna quantos espaços são necessários para linha ser justificada
-- Linha -> Tamanho da maior Linha -> n espaços para ser justificado
quantosEspacos :: String -> Int -> Int
quantosEspacos x tam = tam - (length x - length (separaPalavras x) + 1)

-- Função que separa um texto em uma lista de linhas
-- Texto -> [Lista das Linhas]
separaLinhas :: String -> [String]
separaLinhas a = lines a

-- Função que separa uma linha em uma lista de palavras
-- Linha -> [Lista de Palavras]
separaPalavras :: String -> [String]
separaPalavras a = words a

-- Função que retorna o tamanho da maior linha
-- [Lista de Linhas] -> Tamanho da Maior Linha
tamanhoMaiorLinha :: [String] -> Int
tamanhoMaiorLinha []  = 0
tamanhoMaiorLinha [a] = length a
tamanhoMaiorLinha (x:xs)
  | length x > tamanhoMaiorLinha xs = length x
  | otherwise = tamanhoMaiorLinha xs

-- Função que insere espaços entre uma lista de palavras
-- [Lista de Palavras] -> N Espaços a serem inseridos -> M espaços que precisam de +1 espaço -> Linha Justificada
insereEspacosPalavras :: [String] -> Int -> Int -> String
insereEspacosPalavras [] a b  = []
insereEspacosPalavras [x] a b = x
insereEspacosPalavras (x:xs:xss) a b
  | b /= 0 = x ++ insereEspacosPalavras (insereEspacos xs (a+1):xss) a (b-1)
  | otherwise = x ++ insereEspacosPalavras (insereEspacos xs a :xss) a 0

-- Função utilizada pra justificar a Lista de Linhas
-- [Lista de Linhas] -> Tamanho da maior linha -> Texto Justificado
espacosNasLinhas :: [String] -> Int -> String
espacosNasLinhas [] a     = ""
espacosNasLinhas [x] a = x ++ "\n"
espacosNasLinhas (x:xs) a
  | (length (words x)) - 1 >= 0 = insereEspacosPalavras (separaPalavras x) inteiro resto ++ "\n" ++ espacosNasLinhas xs a
  | otherwise = x ++ "\n" ++ espacosNasLinhas xs a -- Impedir divisão por 0
  where
    inteiro = quantidadeInteira (quantosEspacos x a) (length (words x) - 1)
    resto = quantidadeResto (quantosEspacos x a) (length (words x) - 1)

-- Função que adiciona espaços na frente da palavra
-- Palavra -> N Espaços -> Palavra com N espaços na frente
insereEspacos :: String -> Int -> String
insereEspacos [] n = []
insereEspacos x  0 = x
insereEspacos x  n = ' ' : insereEspacos x (n-1)

-- Função que retorna o texto sem consertado
-- Texto não consertado - Texto consertado
consertaTexto :: String -> String
consertaTexto x = consertaLinhas (separaLinhas x)

-- Função que conserta as linhas do texto
-- [Lista de Linhas] -> Texto Completo
consertaLinhas :: [String] -> String
consertaLinhas [] = ""
consertaLinhas (x:xs) = consertaPalavras (separaPalavras x) ++ "\n" ++ consertaLinhas xs

-- Função que conserta as palavras do texto
-- [Lista de Palavras] -> Linha Consertada
consertaPalavras :: [String] -> String
consertaPalavras [] = ""
consertaPalavras [x] = x
consertaPalavras (x:xs:xss) = x ++ " " ++ consertaPalavras (xs:xss)

-- Texto base usado no pdf do trabalho
texto :: String
texto = "RUBIÃO fitava a enseada -- eram oito horas da manhã.\n\
\Quem o visse com os polegares metidos no cordão do chambre à janela de uma\n\
\grande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\n\
\quieta mas em verdade vos digo que pensava em outra coisa.\n\
\Cotejava o passado com o presente. Que era há um ano?\n\
\Professor. Que é agora? Capitalista! Olha para si para as chinelas\n\
\(umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\n\
\para o jardim para a enseada para os morros e para o céu e tudo desde as chinelas\n\
\até o céu tudo entra na mesma sensação de propriedade.\n"

-- Texto de teste
teste1 :: String
teste1 = "oi\n\
\oi oi\n\
\oi oi oi\n\
\oi oi oi oi\n\
\oi oi oi oi oi\n\
\oi oi oi oi oi oi\n\
\oi oi oi oi oi oi oi\n\
\oi oi oi oi oi oi oi oi\n\
\oi oi oi oi oi oi oi oi oi\n\
\oi oi oi oi oi oi oi oi oi oi\n\
\oi oi oi oi oi oi oi oi oi oi oi\n"

-- Texto de Teste 2
teste2 :: String
teste2 = "The path of the righteous man is beset on all sides\n\
\by the inequities of the selfish and the tyranny of evil men.\n\
\Blessed is he who, in the name of charity and good will,\n\
\shepherds the weak through the valley of darkness,\n\
\for he is truly his brother's keeper and the finder of lost children.\n\
\And I will strike down upon thee with great vengeance\n\
\and furious anger those who attempt to poison and destroy my brothers.\n\
\And you will know my name is the Lord when I lay my vengeance upon thee!\n\
\Ezekiel 25:17.\n"

teste3 :: String
teste3 = "O mundo não é um grande arco-íris\n\
\é um lugar sujo, um lugar cruel,\n\
\que não quer saber o quanto você é durão.\n\
\Vai botar você de joelhos\n\
\e você vai ficar de joelhos para sempre se você deixar.\n\
\Você, eu, ninguém vai bater tão duro como a vida,\n\
\mas não se trata de bater duro.\n\
\Se trata de quanto você aguenta apanhar e seguir em frente,\n\
\o quanto você é capaz de aguentar e continuar tentando.\n\
\É assim que se consegue vencer.\n\
\Agora se você sabe do teu valor,\n\
\então vá atrás do que você merece,\n\
\mas tem que ter disposição para apanhar.\n\
\E nada de apontar dedos,\n\
\dizer que você não consegue por causa dele ou dela,\n\
\ou de quem quer que seja.\n\
\Só covardes fazem isso e você não é covarde,\n\
\você é melhor que isso.\n"

teste4 :: String
teste4 = "General Kenobi. Years ago you served my father in the Clone\n\
\Wars. Now he begs you to help him in his struggle against the\n\
\Empire. I regret that I am unable to present my father's\n\
\request to you in person, but my ship has fallen under attack,\n\
\and I'm afraid my mission to bring you to Alderaan has failed.\n\
\I have placed information vital to the survival of the Rebellion\n\
\into the memory systems of this R2 unit. My father will know\n\
\how to retrieve it. You must see this droid safely delivered to\n\
\him on Alderaan. This is our most desperate hour.\n\
\Help me, Obi-Wan Kenobi. You're my only hope.\n"
