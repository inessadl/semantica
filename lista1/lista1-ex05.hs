--5. 	O que está errado com a seguinte definição de todosDiferentes:
--		todosDiferentes n m p = ( ( n/=m ) && ( m/=p ) )
--		O conjunto de testes que você definiu na questão anterior funciona com
--		esta definição? 

todosDiferentes n m p = ( ( n/=m ) && ( m/=p ) )


-- Casos de testes:
--
-- todosDiferentes n1 n2 n3 	10 20 30	True	ok
-- todosDiferentes n1 n2 n4 	10 20 20	False	ok
-- todosDiferentes n2 n4 n5 	20 20 20 	False	ok

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
