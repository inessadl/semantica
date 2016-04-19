--3. 	Implemente a função 		produtoLista :: [Int] -> Int
--		que implementa o produto de uma lista de inteiros.

produtoLista :: [Int] -> Int
produtoLista [] = error "_"			
produtoLista (a:x) = a * produtoLista x