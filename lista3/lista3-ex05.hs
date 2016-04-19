--5.	Implemente a função 		concatLista :: [[Int]] ->[Int]
--		que transforma uma lista de lista de inteiros em uma lista única de inteiros.
--		Ex:		Hugs> concatLista [[1,2,3], [2], [4,5]]
--				[1,2,3,2,4,5]

concatLista :: [[Int]] -> [Int] 
concatLista[] = []
concatLista (a:x) = a ++ concatLista x
