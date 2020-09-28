-- Listas para teste
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

-- Bubble Sort
troca_cont :: (Ord a) => ([a], Int) -> ([a], Int)
troca_cont ([x], n) = ([x], n)
troca_cont ((x:y:xs), n) = if x <= y then add (troca_cont ((x:xs), n+1)) y else add (troca_cont ((y:xs), n+1)) x
    where
        add (list, n) a = (a:list, n)

bubble_cont :: (Ord a) => ([a], Int) -> Int -> ([a], Int)
bubble_cont (l, c) 0 = (l, c)
bubble_cont (l, c) n = bubble_cont (troca_cont (l, c)) (n-1)

bubble_sort3 :: (Ord a) => [a] -> ([a], Int)
bubble_sort3 [] = ([], 0)
bubble_sort3 list = bubble_cont (list, 0) (length list)

-- Selection Sort
max1 :: (Ord a) => [a] -> Int -> (a, Int)
max1 [] _ = undefined
max1 [x] n = (x, n)
max1 (x:y:xs) n = if x <= y then max1 (y:xs) (n+1) else max1 (x:xs) (n+1)

-- n é o numero atual de checagens
selection :: (Ord a) => [a] -> Int -> ([a], Int)
selection [] n = ([], n)
selection (x:xs) n = 
    let
        (least, n_num) = max1 (x:xs) n

        remove _ [] = []
        remove n (h:t) = if n == h then t else h:(remove n t)

        add (lst, n) y = (y:lst, n)
    in
        add (selection (remove least (x:xs)) n_num) least

selection_sort3 :: (Ord a) => [a] -> ([a], Int)
selection_sort3 lst = selection lst 0

-- Insertion Sort
insereOrd_cont :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insereOrd_cont x [] n = ([x], n)
insereOrd_cont x (h:t) n = if x > h then ((x:h:t), n+1) else add (insereOrd_cont x t (n+1)) h
    where
        add (list, n) y = (y:list, n)

insertion_sort3 :: (Ord a) => [a] -> ([a], Int)
insertion_sort3 [] = ([], 0)
insertion_sort3 [x] = ([x], 0)
insertion_sort3 (h:t) =
    let
        (sorted_tail, n) = insertion_sort3 t

        (lst, n1) = insereOrd_cont h sorted_tail n
    in
        (lst, n1)

-- Quick Sort
filter_with_count :: [a] -> Int -> (a -> Bool) -> ([a], Int)
filter_with_count [] n _ = ([], n)
filter_with_count (x:xs) n cond = if cond x then add (filter_with_count xs (n+1) cond) x else filter_with_count xs (n+1) cond
    where
        add (list, n) y = (y:list, n)

quick_sort3 :: (Ord a) => [a] -> ([a], Int)
quick_sort3 [] = ([], 0)
quick_sort3 (piv:xs) =
    let
        (left, n_L) = filter_with_count xs 0 (> piv)
        (right, n_R) = filter_with_count xs 0 (<= piv)

        (sorted_L, n1_L) = quick_sort3 left
        (sorted_R, n1_R) = quick_sort3 right
    in
        (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)