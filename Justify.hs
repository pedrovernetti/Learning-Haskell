
-- ---------------------------------------------------------------------
-- O objetivo do trabalho é implementar a função justifica:
-- justifica :: String -> String
-- que recebe como entrada uma string contendo um texto em várias linhas
-- e devolve o mesmo texto justificado pela maior linha.


-- ---------------------------------------------------------------------
-- textos usados para teste
-- ---------------------------------------------------------------------

-- trecho de Quincas Borba (Machado de Assis)
text1 =  "RUBIAO fitava a enseada eram oito horas da manha.\n\
\Quem o visse com os polegares metidos no cordao do chambre a janela de uma\n\
\grande casa de Botafogo cuidaria que ele admirava aquele pedaco de agua\n\
\quieta mas em verdade vos digo que pensava em outra coisa.\n\
\Cotejava o passado com o presente. Que era ha um ano?\n\
\rofessor. Que e agora? Capitalista! Olha para si para as chinelas\n\
\(umas chinelas de Tunis que lhe deu recente amigo Cristiano Palha) para a casa\n\
\para o jardim para a enseada para os morros e para o ceu e tudo desde as chinelas\n\
\até o ceu tudo entra na mesma sensacao de propriedade.\n"

-- trecho de Ato Final do Teatro da Existência (autoria própria)
text2 = "esquecer de ser um simples mortal humano,\n\
\basicamente animal, cegamente mediano,\n\
\meramente carnal, naturalmente tirano\n\
\e ser agora, apenas total e essencialmente, insano… profano…\n\
\e então, aqui, se encerra o meu teatro da existência\n\
\aqui e agora, deixo minha essência absorver o espaço…\n\
\o espaço dissolver minha essência…\n\
\no ato final da forma humana e animal\n"

-- trecho da Alegoria da Caverna (Platão)
text3 = "Agora, imagina a maneira como segue o estado da nossa natureza\n\
\relativamente à instrução e à ignorância. Imagina homens numa morada\n\
\subterrânea, em forma de caverna, com uma entrada aberta à luz;\n\
\esses homens estão aí desde a infância, de pernas e pescoços\n\
\acorrentados, de modo que não podem mexer-se nem ver senão o que está\n\
\diante deles, pois as correntes os impedem de voltar a cabeça; a luz\n\
\chega-lhes de uma fogueira acesa numa colina que se ergue por detrás\n\
\deles; entre o fogo e os prisioneiros passa uma estrada ascendente.\n\
\Imagina que ao longo dessa estrada está construído um pequeno muro,\n\
\semelhante às divisórias que os apresentadores de títeres armam\n\
\diante de si e por cima das quais exibem as suas maravilhas.\n"

-- ---------------------------------------------------------------------
-- implementação
-- ---------------------------------------------------------------------

-- função para simplificar o uso da função justify com putStr no GHCi
formatedJustify :: String -> IO()
formatedJustify text = putStr(justify text)

-- justifica um texto, adicionando espaços em cada linha para que fiquem
-- com o comprimento da maior linha
justify :: String -> String
justify [] = []
justify text = concat (justify' (splitStr text '\n') (longestLineLength text))
justify' :: [String] -> Int -> [String]
justify' [lastLine] _ = [lastLine ++ "\n"]
justify' lines desiredLength = 
            ((justifyLine (lines !! 0) desiredLength) ++ "\n") : 
            (justify' (tail lines) desiredLength)

-- justifica uma linha, adicionando espaços até atingir um comprimento N
justifyLine :: String -> Int -> String
justifyLine line dl -- dl = desired length
   | len line < dl = justifyLine (addSpaces line (dl - (len line))) dl
   | otherwise = line 

-- adiciona N espaços a uma string (sempre onde já há espaços)
addSpaces :: String -> Int -> String
addSpaces line 0 = line
addSpaces [] howMany = nSpaces howMany
addSpaces (firstChar:restOfTheLine) howMany
   | restOfTheLine == [] = [firstChar]
   | firstChar == ' ' = "  " ++ (addSpaces restOfTheLine (howMany - 1))
   | otherwise = firstChar : (addSpaces restOfTheLine howMany)
   
-- retorna uma string com N espaços (apenas)
nSpaces :: Int -> String
nSpaces howMany
   | howMany > 0 = ' ' : (nSpaces (howMany - 1))
   | otherwise = []
   
-- retorna o número de linhas de uma string
linesCount :: String -> Int
linesCount [] = 0
linesCount text = len (splitStr text '\n')

-- retorna o tamanho da sub-lista (ou string) mais comprida de uma lista
longestLineLength :: String -> Int
longestLineLength text = lengthOfTheLongest (splitStr text '\n')

-- retorna o tamanho da sub-lista (ou string) mais comprida de uma lista
lengthOfTheLongest :: [[anytype]] -> Int
lengthOfTheLongest list = greatest (lengthList list)

-- retorna o maior tamanho de uma lista
greatest :: Ord anytype => [anytype] -> anytype
greatest [x] = x
greatest (x:y:tail)
   | x >= y = greatest (x:tail)
   | otherwise = greatest (y:tail)

-- retorna uma lista com os tamanhos de cada sub-lista da lista de entrada
lengthList :: [[anytype]] -> [Int]
lengthList [] = []
lengthList list = len (list !! 0) : lengthList (tail list)

-- retorna o comprimento de uma lista
len :: [anytype] -> Int
len [] = 0
len list = length (tail list) + 1

-- separa strings baseando-se num char separador
-- retorna uma lista dos pedaços da string então separada
splitStr :: String -> Char -> [String]
splitStr s c = reverse (go s []) where
    go s' ws = case (dropWhile (\c' -> c' == c) s') of
        "" -> ws
        rem -> go ((dropWhile (\c' -> c' /= c) rem)) ((takeWhile (\c' -> c' /= c) rem) : ws)
        
