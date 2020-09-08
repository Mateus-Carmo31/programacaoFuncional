-- Exercício 1
lst1 = [x*2 | x <- [1..10], x*2 >= 12]

lst2 = [ x | x <- [50..100], mod x 7 == 3]

lst3 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]

lst4 = [(x,y)| x <- [1..4], y <- [x..5]]

-- Exercício 2
quadrados :: Int -> Int -> [Int]
quadrados a b = [x^2 | x<-[a..b]]

-- Exercício 3
seleciona_impares :: [Int] -> [Int]
seleciona_impares l = [x | x<-l, odd x]

-- Exercício 4
tabuada :: Int -> [Int]
tabuada n = [n*x | x<-[1..10]]

-- Exercício 5
bissexto :: Int -> Bool
bissexto x
 | mod x 4 /= 0   = False
 | mod x 100 /= 0 = True
 | mod x 400 /= 0 = False
 | otherwise      = True

bissextos :: [Int] -> [Int]
bissextos l = [x | x<-l, bissexto x]

-- Exercício 6
sublistas :: [[a]] -> [a]
sublistas l = [x | y<-l, x<-y]

-- Exercício 7
type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo = [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
    ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
    ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

precede :: Data -> Data -> Bool
precede (d1, m1, y1) (d2, m2, y2)
 | y1 > y2 = False
 | y1 == y2 && m1 > m2 = False
 | y1 == y2 && m1 == m2 && d1 > d2 = False
 | otherwise = True

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados lst hj = [emp | emp<-lst, precede hj (get_data_devol emp) == False]
    where get_data_devol (_, _, _, d, _) = d

-- Exercício 8
nPares_aux :: [Int] -> Int
nPares_aux [] = 0
nPares_aux [x] = x
nPares_aux (counter:x:xs) = if even x then nPares_aux ((counter+1):xs) else nPares_aux (counter:xs)

nPares :: [Int] -> Int
nPares lst = nPares_aux (0:lst)

-- Exercício 9
produtorio :: Num a => [a] -> a
produtorio [] = 0
produtorio [x] = x
produtorio (x:y:xs) = produtorio ((x*y):xs)

-- Exercício 10
comprime :: [[a]] -> [a]
comprime [[]] = []
comprime ([]:ys) = comprime ys
comprime ((x:xs):ys) = x:(comprime (xs:ys))

-- Exercício 11
tamanho :: [a] -> Int
tamanho lst = tamanho_aux lst 0
    where
        tamanho_aux [] i = i
        tamanho_aux (x:xs) i = tamanho_aux xs (i+1)

-- Exercício 12
uniaoNRec :: Eq a => [a] -> [a] -> [a]
uniaoNRec set1 set2 = [x | x<-set1] ++ [y | y<-set2, elem y set1 == False]

-- Exercício 13
remove_todos_elem :: Eq a => a -> [a] -> [a]
remove_todos_elem _ [] = []
remove_todos_elem a (x:xs) = if a == x then remove_todos_elem a xs else x:(remove_todos_elem a xs)

uniaoNRec2 :: Eq a => [a] -> [a] -> [a]
uniaoNRec2 [] [] = []
uniaoNRec2 a [] = a
uniaoNRec2 [] b = b
uniaoNRec2 (a:as) (b:bs)
 | elem a (b:bs) = a:(uniaoNRec2 as (remove_todos_elem a (b:bs)))
 | otherwise = a:(uniaoNRec2 as (b:bs))