somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (a:x) = a + somaLista(x)

-- somaLista[2,3]
-- 5