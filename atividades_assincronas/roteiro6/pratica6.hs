-- Exercício 1
paridade :: [Int] -> [Bool]
paridade l = map (even) l

-- Exercício 2
prefixos :: [String] -> [String]
prefixos l = map (take 3) l

-- Exercício 3
saudacao :: [String] -> [String]
saudacao l = map ("Oi " ++) l

-- Exercício 4
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar func l = [x | x<-l, func x]

filtrar2 :: (a -> Bool) -> [a] -> [a]
filtrar2 _ [] = []
filtrar2 func (x:xs) = if func x then (x:(filtrar func xs)) else filtrar func xs

-- Exercício 5
pares :: [Int] -> [Int]
pares l = filter (even) l

-- Exercício 6
solucoes :: [Int] -> [Int]
solucoes l = filter (\x -> (5*x + 6) < (x*x)) l

-- Exercício 7
maior :: [Int] -> Int
maior l = foldr1 (max) l

-- Exercício 8
menor_min10 :: [Int] -> Int
menor_min10 l = foldr (min) 10 l

-- Exercício 9
junta_silabas_plural :: [String] -> String
junta_silabas_plural l = foldr (++) "s" l

lst1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
lst2 = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
lst3 = [11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
lst4 = [10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
lst5 = [11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
lst6 = [1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
lst7 = [1..1000]
lst8 = [1000,999..1]
lst9 = lst1++[0]
lst10 = [0]++lst3
lst11 = lst1++[0]++lst3
lst12 = lst3++[0]++lst1

-- Exercício 10

-- Bubble Sort
bubble_sort :: (Ord a) => [a] -> [a]
bubble_sort [] = []
bubble_sort list = bubble list (length list)

bubble :: (Ord a) => [a] -> Int -> [a]
bubble list 0 = list
bubble list n = bubble (troca list) (n-1)

troca :: (Ord a) => [a] -> [a]
troca [x] = [x]
troca (x:y:xs) = if x > y then y:troca (x:xs) else x:troca (y:xs)

-- Selection Sort
selection_sort :: (Ord a) => [a] -> [a]
selection_sort [] = []
selection_sort [x] = [x]
selection_sort (x:xs) = 
    let
        least = foldr1 (min) (x:xs)

        remove _ [] = []
        remove n (h:t) = if n == h then t else h:(remove n t)
    in
        [least] ++ selection_sort (remove least (x:xs))

-- Insertion Sort
insereOrd :: (Ord a) => a -> [a] -> [a]
insereOrd x [] = [x]
insereOrd x (h:t) = if x <= h then (x:h:t) else h : (insereOrd x t)

insertion_sort :: (Ord a) => [a] -> [a]
insertion_sort l = foldr (insereOrd) [] l

-- Quick Sort
quick_sort :: (Ord a) => [a] -> [a]
quick_sort [] = []
quick_sort (piv:xs) = (quick_sort [x | x<-xs, x <= piv]) ++ [piv] ++ (quick_sort [x | x<-xs, x > piv])

-- Exercício 11

-- Bubble Sort
troca_cont :: (Ord a) => ([a], Int) -> ([a], Int)
troca_cont ([x], n) = ([x], n)
troca_cont ((x:y:xs), n) = if x > y then add (troca_cont ((x:xs), n+1)) y else add (troca_cont ((y:xs), n+1)) x
    where
        add (list, n) a = (a:list, n)

bubble_cont :: (Ord a) => ([a], Int) -> Int -> ([a], Int)
bubble_cont (l, c) 0 = (l, c)
bubble_cont (l, c) n = bubble_cont (troca_cont (l, c)) (n-1)

bubble_sort2 :: (Ord a) => [a] -> ([a], Int)
bubble_sort2 [] = ([], 0)
bubble_sort2 list = bubble_cont (list, 0) (length list)

-- Selection Sort
min1 :: (Ord a) => [a] -> Int -> (a, Int)
min1 [] _ = undefined
min1 [x] n = (x, n)
min1 (x:y:xs) n = if x > y then min1 (y:xs) (n+1) else min1 (x:xs) (n+1)

-- n é o numero atual de checagens
selection :: (Ord a) => [a] -> Int -> ([a], Int)
selection [] n = ([], n)
selection (x:xs) n = 
    let
        (least, n_num) = min1 (x:xs) n

        remove _ [] = []
        remove n (h:t) = if n == h then t else h:(remove n t)

        add (lst, n) y = (y:lst, n)
    in
        add (selection (remove least (x:xs)) n_num) least

selection_sort2 :: (Ord a) => [a] -> ([a], Int)
selection_sort2 lst = selection lst 0

-- Insertion Sort
insereOrd_cont :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insereOrd_cont x [] n = ([x], n)
insereOrd_cont x (h:t) n = if x <= h then ((x:h:t), n+1) else add (insereOrd_cont x t (n+1)) h
    where
        add (list, n) y = (y:list, n)

insertion_sort2 :: (Ord a) => [a] -> ([a], Int)
insertion_sort2 [] = ([], 0)
insertion_sort2 [x] = ([x], 0)
insertion_sort2 (h:t) =
    let
        (sorted_tail, n) = insertion_sort2 t

        (lst, n1) = insereOrd_cont h sorted_tail n
    in
        (lst, n1)

-- Quick Sort
filter_with_count :: [a] -> Int -> (a -> Bool) -> ([a], Int)
filter_with_count [] n _ = ([], n)
filter_with_count (x:xs) n cond = if cond x then add (filter_with_count xs (n+1) cond) x else filter_with_count xs (n+1) cond
    where
        add (list, n) y = (y:list, n)

quick_sort2 :: (Ord a) => [a] -> ([a], Int)
quick_sort2 [] = ([], 0)
quick_sort2 (piv:xs) =
    let
        (left, n_L) = filter_with_count xs 0 (<= piv)
        (right, n_R) = filter_with_count xs 0 (> piv)

        (sorted_L, n1_L) = quick_sort2 left
        (sorted_R, n1_R) = quick_sort2 right
    in
        (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)

-- Exercício 12
-- Decidi separar em outro arquivo para não precisar lidar com conflitos de nomes
-- Por favor inspecione no arquivo "exercicio12.hs"