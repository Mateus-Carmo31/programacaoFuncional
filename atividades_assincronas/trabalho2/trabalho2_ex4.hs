-- Exercício 4

l1=[1..1000]
l2=[1000,999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7=[20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6] 

-- Variação 1
divide :: (Ord a) => a -> [a] -> ([a], [a])
divide x [] = ([],[])
divide x [e] = if e < x then ([e], []) else ([], [e])
divide x (e:es)
    | e < x = add_left e (divide x es)
    | otherwise = add_right e (divide x es)
    where
        add_left a (l, r) = (a:l, r)
        add_right a (l, r) = (l, a:r)

quick_sort :: (Ord a) => [a] -> [a]
quick_sort [] = []
quick_sort (piv:xs) =
    let
        (left, right) = divide piv xs
    
    in
        (quick_sort left) ++ [piv] ++ (quick_sort right)

-- Variação 2
quick_sort2 :: (Ord a) => [a] -> [a]
quick_sort2 [] = []
quick_sort2 lst =
    let
        first_three = take 3 lst
        piv = if length (first_three) < 3 then first_three !! 0 else foldr1 (min) (first_three)

        delete_first_occurance x [] = []
        delete_first_occurance x (y:ys)
            | x == y = ys
            | otherwise = y : delete_first_occurance x ys
        
        (left, right) = divide piv (delete_first_occurance piv lst)
    
    in
        (quick_sort2 left) ++ [piv] ++ (quick_sort2 right)

-- COM CONTAGEM
divide_cont :: (Ord a) => a -> [a] -> Int -> ([a], [a], Int)
divide_cont x [] n = ([],[], n)
divide_cont x [e] n = if e < x then ([e], [], n+1) else ([], [e], n+1)
divide_cont x (e:es) n
    | e < x = add_left e (divide_cont x es (n+1))
    | otherwise = add_right e (divide_cont x es (n+1))
    where
        add_left a (l, r, c) = (a:l, r, c)
        add_right a (l, r, c) = (l, a:r, c)

quick_sort_cont :: (Ord a) => [a] -> ([a], Int)
quick_sort_cont [] = ([], 0)
quick_sort_cont (piv:xs) =
    let
        (left, right, n) = divide_cont piv xs 0

        (sortedL, n_L) = quick_sort_cont left
        (sortedR, n_R) = quick_sort_cont right
    
    in
        (sortedL ++ [piv] ++ sortedR, n + n_L + n_R)

quick_sort2_cont :: (Ord a) => [a] -> ([a], Int)
quick_sort2_cont [] = ([], 0)
quick_sort2_cont lst =
    let
        piv = foldr1 (min) (take 3 lst)

        delete_first_occurance :: (Ord a) => a -> [a] -> Int -> ([a], Int)
        delete_first_occurance x [] n = ([], n)
        delete_first_occurance x (y:ys) n
            | x == y = (ys, n+1)
            | otherwise = add y (delete_first_occurance x ys (n+1))
            where
                add e (l, c) = (e:l, c) 
        
        (new_lst, checks) = delete_first_occurance piv lst 0

        (left, right, n1) = divide_cont piv new_lst 0
        (sortedL, n_L) = quick_sort2_cont left
        (sortedR, n_R) = quick_sort2_cont right

    in
        (sortedL ++ [piv] ++ sortedR, n1 + n_L + n_R + checks + 3) -- Comps. atuais + comps recursivas + comps do delete_first_occurance + 3 comps. do foldr1

{- 
Eu diria que a melhor variação do algoritmo do quicksort, entre essas duas,
é a primeira. As duas variações são ambas extremamente similares, sendo que
a segunda só varia na escolha de pivo, que, apesar de poder acelerar auxiliar
no número de iterações necessárias para completar o sorteamento, acaba
adicionando um peso extra de comparações para reorganizar a lista para que tal
escolha de pivo funcione bem (na minha implementação, a adição da função
'delete_first_occurance'). Por isso, a primeira variação aparenta-se mais
eficiente e garantida em termos de perfomace.
 -}