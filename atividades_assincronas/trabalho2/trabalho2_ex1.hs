-- Trabalho 2 de PF
-- Mateus Carmo de Oliveira (11911BCC026)

-- PARTE 1: ALGORITMOS DE ORDENAÇÃO

-- Exercício 1
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
insertion_sort :: (Ord a) => [a] -> [a]
insertion_sort l = foldr (insereOrd) [] l
    where
        insereOrd x [] = [x]
        insereOrd x (h:t) = if x <= h then (x:h:t) else h : (insereOrd x t)

-- Quick Sort
quick_sort :: (Ord a) => [a] -> [a]
quick_sort [] = []
quick_sort (piv:xs) = quick_sort (filter (<piv) xs) ++ [piv] ++ quick_sort (filter (>=piv) xs)