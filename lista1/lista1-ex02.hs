-- 2. 	Defina a função quantosSaoIguais :: Int -> Int -> Int -> Int que
-- 		conta quantos argumentos iguais a fun ̧c ̃ao recebeu

quantosSaoIguais :: Int -> Int -> Int -> Int -> Int
quantosSaoIguais a b c d
				| (a /= b) && (b /= c) && (c /= d) = 0
				| (a == b) && (b /= c) && (c /= d) = 2
				| (a /= b) && (b == c) && (c /= d) = 2				
				| (a /= b) && (a /= c) && (c == d) = 2
				| (a == b) && (b == c) && (c /= d) = 3
				| (a /= b) && (b == c) && (c == d) = 3
				| (a == b) && (b /= c) && (a == d) = 3
				| otherwise = 4