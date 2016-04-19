--6. 	Implemente a função		inverteLista :: [Int] -> [Int]
--		Ex.
--			Hugs> inverteLista [1,2,3,4]
--			[4,3,2,1]

inverteLista :: [Int] -> [Int]
inverteLista[] = []
inverteLista[a] = [a]
inverteLista(a:x) =  inverteLista x ++ [a] 