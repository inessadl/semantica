--2.	Implemente a função 	tamanho :: [Int] -> Int
--		que conta o número de elementos de uma lista

tamanho :: [Int] -> Int
tamanho[] = 0
tamanho(a:x) = 1 + tamanho x