--1.  Defina uma função   max :: Int -> Int -> Int
--    que retorna o maior entre dois números.

max :: Int -> Int -> Int
max a b
  | a <= b = b
  | a > b = a
