-- Exercício 3

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
selection_sort :: (Ord a) => [a] -> [a]
selection_sort [] = []
selection_sort [x] = [x]
selection_sort (x:xs) = 
    let
        least = foldr1 (min) (x:xs)

        remove _ [] = []
        remove n (h:t) = if n == h then t else h:(remove n t)
    in
        least : selection_sort (remove least (x:xs))

-- Variação 2
-- selection_sort2 :: (Ord a) => [a] -> [a]
-- selection_sort2 [] = []
-- selection_sort2 (x:xs) =
--     let
--         split_at l n = (take n l, l !! n, drop (n+1) l)

--         encontra_menor_idx l c idx
--             | c == (length l) = idx
--             | (l !! c) < (l !! idx) = encontra_menor_idx l (c+1) c
--             | otherwise = encontra_menor_idx l (c+1) idx
        
--         least_id = encontra_menor_idx (x:xs) 0 0

--         (left, least, right) = split_at (x:xs) least_id

--     in
--         least:(selection_sort2 (left ++ right))

remove_menor :: (Ord a) => (a, [a]) -> (a, [a])
remove_menor (m, [x]) = if x < m then (x, [m]) else (m, [x])
remove_menor (menor, (x:xs))
    | x < fst (remove_menor (menor, xs)) = (x, menor:xs)
    | otherwise = add (remove_menor (x, xs)) menor
    where
        add a (n, l) = (n, a:l)