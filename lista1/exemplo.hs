idade :: Int
idade = 18

maiorDeIdade :: Int -> Bool
maiorDeIdade x = x >= idade

quadrado :: Int -> Int
quadrado x = x * x

mini :: Int -> Int -> Int 
mini a b 
	| a <= b = 4
	| otherwise = b

tresIguais :: Int -> Int -> Int -> Bool
tresIguais x y z = (x == y) && (x == z)  
