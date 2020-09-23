-- Exercício 1

type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo::Emprestimos
bdEmprestimo = [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
    ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
    ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

-- Letra A
valida :: Data -> Bool
valida (d, m, y)
 | out_of_bounds (d,m,y) = False
 | m == 2 = (if (bissexto y) then not(d > 29) else not(d > 28))
 | (m == 4 || m == 6 || m == 9 || m == 11) && d > 30 = False
 | otherwise = True
 where
     out_of_bounds (d, m, y) = d < 1 || d > 31 || m < 1 || m > 12
     bissexto x
        | mod x 4 /= 0   = False
        | mod x 100 /= 0 = True
        | mod x 400 /= 0 = False
        | otherwise      = True

-- Letra B
bissextos :: [Int] -> [Int]
bissextos [] = []
bissextos (x:xs) = if (bissexto x) then (x:bissextos(xs)) else bissextos(xs)
    where
        bissexto x
            | mod x 4 /= 0   = False
            | mod x 100 /= 0 = True
            | mod x 400 /= 0 = False
            | otherwise      = True

-- Letra C
atrasados :: Emprestimos -> Data -> Emprestimos
atrasados [] _ = []
atrasados (x:xs) d = if (atrasado x d) then (x:(atrasados xs d)) else (atrasados xs d)
    where
        precede (d1, m1, y1) (d2, m2, y2)
            | not(valida (d1, m1, y1) && valida (d2,m2,y2)) = False
            | y1 > y2 = False
            | y1 == y2 && m1 > m2 = False
            | y1 == y2 && m1 == m2 && d1 > d2 = False
            | otherwise = True
        
        atrasado (_, _, _, d_devol, status) data_hj = not(precede data_hj d_devol)

-- Letra D
fibo2_aux :: Int -> (Int, Int)
fibo2_aux 0 = (0,1)
fibo2_aux n = passo (fibo2_aux (n-1))
    where
        passo (x, y) = (y, x+y)

fibo2 :: Int -> Int
fibo2 n = snd (fibo2_aux n)

-- Letra E
fatorial :: Int -> Int
fatorial n = prodIntervalo 1 n
    where
        prodIntervalo m n
            | m >= n = n
            | otherwise = m * (prodIntervalo (m+1) n)

-- Exercício 2

-- Letra A
valida2 :: Data -> Bool
valida2 (d, m, y) = 
    let
        out_of_bounds (d, m, y) = d < 1 || d > 31 || m < 1 || m > 12
        bissexto x
            | mod x 4 /= 0   = False
            | mod x 100 /= 0 = True
            | mod x 400 /= 0 = False
            | otherwise      = True
        
        teste1 = not(out_of_bounds (d, m, y))
        teste2 = not(m == 2 && (if (bissexto y) then (d>29) else (d>28)))
        teste3 = not((m == 4 || m == 6 || m == 9 || m == 11) && d > 30)
    
    in
        teste1 && teste2 && teste3

-- Letra B
bissextos2 :: [Int] -> [Int]
bissextos2 list =
    let
        bissexto x
            | mod x 4 /= 0   = False
            | mod x 100 /= 0 = True
            | mod x 400 /= 0 = False
            | otherwise      = True
    
    in
        [x | x<-list, bissexto x]

-- Letra C
atrasados2 :: Emprestimos -> Data -> Emprestimos
atrasados2 emps data_hj =
    let
        precede (d1, m1, y1) (d2, m2, y2)
            | not(valida (d1, m1, y1) && valida (d2,m2,y2)) = False
            | y1 > y2 = False
            | y1 == y2 && m1 > m2 = False
            | y1 == y2 && m1 == m2 && d1 > d2 = False
            | otherwise = True
        
        get_data_entrega (_, _, _, d, _) = d
    in
        [emp | emp<-emps, precede (get_data_entrega emp) data_hj]

-- Letra D
fibo2_2 :: Int -> Int
fibo2_2 n =
    let
        passo (x, y) = (y, x+y)

        fibo2_aux 0 = (0,1)
        fibo2_aux x = passo(fibo2_aux (x-1))
    
    in
        snd (fibo2_aux n)

-- Letra E
fatorial2 :: Int -> Int
fatorial2 n =
    let
        prodIntervalo a b
            | a >= b = b
            | otherwise = a * (prodIntervalo (a+1) b)
    
    in
        prodIntervalo 1 n

-- Exercício 5
letraA = (\x -> \y -> y)((\z -> z)(\z -> z))(\w -> w) 5

letraB = ((\f -> (\x -> f(f x))) (\y -> y * y)) 3

letraC = ((\f -> (\x -> f(f x))) (\y -> (+) y y)) 5

letraD = ((\x -> (\y -> (+) x y) 5) ((\y -> (-) y 3) 7))

letraE = (((\f -> (\x -> f(f(f x)))) (\y -> (y * y))) 2)

letraF = (\x -> \y -> (+) x ((\x -> (-) x 3) y)) 5 6