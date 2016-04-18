-- 1. 	Escreva a função osQuatroSaoIguais que possui tipo:
--		Int -> Int -> Int -> Int -> Bool

osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais a b c d = (a == b) && (b == c) && (c == d)
