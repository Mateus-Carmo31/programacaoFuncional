-- EXERCICIO 1
-- LETRA A
ou1 :: Bool -> Bool -> Bool
ou1 True True = True
ou1 True False = True
ou1 False True = True
ou1 False False = False

ou2 :: Bool -> Bool -> Bool
ou2 True _ = True
ou2 _ True = True
ou2 _ _ = False

ou3 :: Bool -> Bool -> Bool
ou3 False b = b
ou3 True _ = True

-- LETRA B
ou4 :: Bool -> Bool -> Bool
ou4 a b
 | a == True = True
 | b == True = True
 | otherwise = False

ou5 :: Bool -> Bool -> Bool
ou5 a b = if a == False && b == False then False else True

-- EXERCICIO 2
dist :: (Float, Float) -> (Float, Float) -> Float
dist (x1,y1) (x2,y2) = sqrt ((x2-x1)**2 + (y2-y1)**2)

-- EXERCICIO 4
fat1 :: Int -> Int
fat1 n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = n * fat1 (n-1)

fat2 :: Int -> Int
fat2 0 = 1
fat2 1 = 1
fat2 n = n * fat2 (n-1)

-- EXERCICIO 5
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

-- EXERCICIO 6
n_tri :: Int -> Int
n_tri 0 = 0
n_tri 1 = 1
n_tri n = n + n_tri (n-1)

-- EXERCICIO 7
passo :: (Int, Int) -> (Int, Int)
passo (x, y) = (y, x+y)

fibo2 :: Int -> (Int, Int)
fibo2 0 = (0,1)
fibo2 n = passo (fibo2 (n-1))

-- EXERCICIO 8
potencia2 :: Int -> Int
potencia2 0 = 1
potencia2 1 = 2
potencia2 n = potencia2 (n-1) * 2

-- EXERCICIO 9
-- Letra a
prodIntervalo :: Int -> Int -> Int
prodIntervalo m n
 | m >= n = n
 | otherwise = m * (prodIntervalo (m+1) n)

-- Letra b
fatInter :: Int -> Int
fatInter n = prodIntervalo 1 n

-- EXERCICIO 11
div_inteira :: Int -> Int -> Int
div_inteira a b
 | a < b = 0
 | otherwise = 1 + div_inteira (a-b) b

resto_div :: Int -> Int -> Int
resto_div a b
 | a < b = a
 | otherwise = resto_div (a-b) b

-- EXERCICIO 12
mdc1 :: Int -> Int -> Int
mdc1 m n
 | n == 0 = m
 | otherwise = mdc1 n (mod m n)

mdc2 :: Int -> Int -> Int
mdc2 m 0 = m
mdc2 m n = mdc2 n (mod m n)

-- EXERCICIO 13
binomial1 :: Int -> Int -> Int
binomial1 n k
 | k > n || k < 0 = -1
 | k == n = 1
 | k == 0 = 1
 | otherwise = (binomial1 (n-1) k) + (binomial1 (n-1) (k-1))

binomial2 :: Int -> Int -> Int
binomial2 n 0 = 1
binomial2 n k = if k == n then 1 else (binomial2 (n-1) k) + (binomial2 (n-1) (k-1))

-- EXERCICIO 15
-- Letra A
intervalo :: Int -> Int -> [Int]
intervalo a b = [a..b]

-- Letra B
intervaloAberto :: Int -> Int -> [Int]
intervaloAberto a b = [(a+1)..(b-1)]