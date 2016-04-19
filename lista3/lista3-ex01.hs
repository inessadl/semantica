--1. 	Implemente a função		dobraLista :: [Int] -> [Int]
--		que dobra o valor de todos os elementos de uma lista

dobraLista :: [Int] -> [Int]
dobraLista [] = error "_"
dobraLista (a:x) = a * 2 : dobraLista x