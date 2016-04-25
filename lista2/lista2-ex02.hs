--2.    Usando a função max, defina uma função maiorVenda que recebe um
--      argumento numérico n, e calcule a maior venda em uma semana entre 0 e n.

max :: Int -> Int -> Int
max a b
  | a < b = b
  | a > b = a

vendas :: Int -> Int
vendas n
	| n == 0 = 10
	| n == 1 = 11
	| n == 2 = 12
	| otherwise = 20

maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0
maiorVenda 1 = vendas 1 
-- maiorVenda (a:x) = max (maiorVenda 0)
