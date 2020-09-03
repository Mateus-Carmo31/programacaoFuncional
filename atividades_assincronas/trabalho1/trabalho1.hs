-- Trabalho 1 de PF (GBC033)
-- Grupo: Mateus Carmo e Nayara
-- Exercício 1
triangulo :: Float -> Float -> Float -> [Char]
triangulo a b c
  |(a + b + c) /= 180 || a == 0 || b == 0 || c == 0 = "nao_triangulo"
  |a == b && a == c = "equilatero"
  |a == 90 || b == 90 || c == 90 = "retangulo"
  |a > 90 || b > 90 || c > 90 = "obtuso"
  |otherwise = "simples"

-- Exercício 2
bhaskara :: Float -> Float -> Float -> (Float, Float)
bhaskara a b c = ((-b + sqrt(b**2 - 4*a*c)) / 2*a, (-b - sqrt(b**2 - 4*a*c)) / 2*a)

equacao :: Float -> Float -> Float -> (Float, Float)
equacao a b c
  | a == 0 = (-c / b, a)
  | otherwise = bhaskara a b c

-- Exercício 3
type Data = (Int, Int, Int)

defineIdade :: Data -> Data -> Int
defineIdade (dt, mt, yt) (db, mb, yb)
 | mt < mb = yt - yb - 1
 | mt == mb && dt < db = yt - yb - 1
 | otherwise = yt - yb

preco_onibus :: Float -> Data -> Data -> Float
preco_onibus valor hj dob
 | defineIdade hj dob <= 2 = valor * 0.15
 | defineIdade hj dob <= 10 = valor * 0.4
 | defineIdade hj dob >= 70 = valor * 0.5
 | otherwise = valor

-- Exercício 4
gera1 :: [Int]
gera1 = [ x^2 | x<-[1..15], x >= 4, x <= 14]

gera2 :: [(Int,Int)]
gera2 = [ (x,y) | x<-[1..15], x >= 1, x <= 4, y<-[x..2*x]]

gera3 :: [Int]
gera3 = [ x | y<-[1..15], y >= 10, x<-[1..y]]

gera4 :: [(Int, Int)]
gera4 = [ (x,x+1) | x<-[1..15], odd x]

gera5 :: [Int]
gera5 = [ x+y | (x,y)<-gera4 ]

-- Exercício 5
-- Letra A
contaNegM2 :: [Int] -> Int
contaNegM2 list = length [ x | x<-list, x<0, mod x 2 == 0]

-- Letra B
listaNegM2 :: [Int] -> [Int]
listaNegM2 list = [ x | x<-list, x<0, mod x 2 == 0]

-- Exercício 6
distancias_comp :: [(Float, Float)] -> [Float]
distancias_comp lista = [sqrt(x^2 + y^2) | (x,y)<-lista]

-- Exercício 7
fatores :: Int -> [Int]
fatores n = [x | x<-[1..n], x <= n, mod n x == 0]

primos :: Int -> Int -> [Int]
primos a b = [n | n<-[a..b], fatores n == [1, n]]

-- Exercício 8
mdc :: Int -> Int -> Int
mdc m 0 = m
mdc m n = mdc n (mod m n)

mmc2 :: Int -> Int -> Int
mmc2 a b = div (a * b) (mdc a b)

mmc :: Int -> Int -> Int -> Int
mmc a b c = mmc2 a (mmc2 b c)

-- Exercício 9
serie :: Float -> Int -> Float
serie x n
 | n == 1 = 1 / x
 | even n = (x / fromIntegral(n)) + (serie x (n-1))
 | otherwise = (fromIntegral(n) / x) + (serie x (n-1))

-- Exercício 10
fizz_or_buzz :: Int -> String
fizz_or_buzz i
 | mod i 3 == 0 && mod i 5 == 0 = "FizzBuzz"
 | mod i 3 == 0 = "Fizz"
 | mod i 5 == 0 = "Buzz"
 | otherwise = "No"

fizzbuzz :: Int -> [String]
fizzbuzz n = [fizz_or_buzz i | i<-[1..n]]

-- Exercício 11
-- conta_ocorrencias :: Int -> Int -> [Int] -> (Int, Int)
-- conta_ocorrencias a _ [a] = (1,0)
-- conta_ocorrencias _ b [b] = (0,1)
-- conta_ocorrencias a b x:y:tail
--  | a == x = conta_ocorrencias a b [y:tail]

-- Exercício 12
encontrada :: Int -> [Int] -> Bool
encontrada n [] = True
encontrada n (x:xs) = if n == x then False else (encontrada n xs)

unica_ocorrencia :: Int -> [Int] -> Bool
unica_ocorrencia n [] = False
unica_ocorrencia n (x:xs) = if n == x then encontrada n xs else unica_ocorrencia n xs

-- Exercício 13
intercala :: [Int] -> [Int] -> [Int]
intercala [] l2 = l2
intercala l1 [] = l1
intercala (x1:x1s) (x2:x2s) = x1:x2:(intercala x1s x2s)

-- Exercício 14
type Contato = (String, String, Int, String)

recuperar_nome :: String -> [Contato] -> String
recuperar_nome email [] = "Email desconhecido"
recuperar_nome email ((nome, _, _, em):tail)
 | em == email = nome
 | otherwise = recuperar_nome email tail

-- Exercício 15
type Pessoa = (String, Float, Int, Char)

pessoas :: [Pessoa]
pessoas = [ ("Rosa", 1.66, 27,'F'),
    ("João", 1.85, 26, 'C'),
    ("Maria", 1.55, 62, 'S'),
    ("Jose", 1.78, 42, 'C'),
    ("Paulo", 1.93, 25, 'S'),
    ("Clara", 1.70, 33, 'C'),
    ("Bob", 1.45, 21, 'C'),
    ("Rosana", 1.58, 39, 'S'),
    ("Daniel", 1.74, 72, 'S'),
    ("Jocileide", 1.69, 18, 'S') ]

altura_media :: [Pessoa] -> Float
altura_media list = (sum [alt | (_, alt, _, _)<-list]) / fromIntegral(length list)

idade_mais_nova :: [Pessoa] -> Int
idade_mais_nova list = minimum [idade | (_, _, idade, _)<-list]

pessoa_mais_velha :: [Pessoa] -> String
pessoa_mais_velha [(nome, _, _, est_civil)] = nome ++ " (" ++ [est_civil] ++ ")"
pessoa_mais_velha ((n1,a1,i1,e1):(n2,a2,i2,e2):t)
 | i1 >= i2 = pessoa_mais_velha ((n1,a1,i1,e1):t)
 | otherwise = pessoa_mais_velha ((n2,a2,i2,e2):t)

cinquenta_ou_mais :: [Pessoa] -> [Pessoa]
cinquenta_ou_mais lista = [(n,a,i,e) | (n,a,i,e)<-lista, i >= 50]

casadas :: Int -> [Pessoa] -> Int
casadas idade lista = length [(n,a,i,e) | (n,a,i,e)<-lista, i > idade, e == 'C']

-- Exercício 16
insere_ord :: Ord a => a -> [a] -> [a]
insere_ord elem [] = [elem]
insere_ord elem (x:xs) = if x >= elem then elem:x:xs else x:(insere_ord elem xs)

-- Exercício 17
reverte :: [a] -> [a]
reverte [] = []
reverte [a] = [a]
reverte (x:xs) = (reverte xs) ++ [x]
