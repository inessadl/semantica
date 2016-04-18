--4. 	Defina um conjunto de testes para a função todosDiferentes

todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c = (a /= b) && (b /= c)

-- Casos de testes:
--
-- todosDiferentes n1 n2 n3 	10 20 30	True
-- todosDiferentes n1 n2 n4 	10 20 20	False
-- todosDiferentes n2 n4 n5 	20 20 20 	False

n1 :: Int
n1 = 10

n2 :: Int
n2 = 20

n3 :: Int
n3 = 30

n4 :: Int
n4 = 20

n5 :: Int
n5 = 20
