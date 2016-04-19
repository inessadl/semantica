--7. 	Escreva uma definição de quantosSaoIguais que use a função todosDiferentes
--		e a função todosIguais

todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c = (a /= b) && (a /= c)

todosIguais :: Int -> Int -> Int -> Bool
todosIguais a b c = (a == b) && (b == c)

quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais a b c
        | todosDiferentes a b c == True = 0
        | todosIguais a b c == True = 3
        | (a == b) && (b /= c) = 2
        | (a == c) && (b /= c) = 2
        | (b == c) && (b /= a) = 2
        | otherwise = 1
