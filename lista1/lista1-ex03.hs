--3. 	Defina a função  todosDiferentes :: Int -> Int -> Int -> Bool
--		que retorna True se todos os seus argumentos são diferentes.

todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c = (a /= b) && (b /= c)
