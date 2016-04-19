--4. 	Implemente a função 	andLista :: [Bool] -> Bool
--		que faz um and (&&) entre todos os elementos de uma lista

andLista :: [Bool] -> Bool
andLista[] = error "_"
andLista[False] = False
andLista[True] = True
andLista (a:x) = a && andLista x