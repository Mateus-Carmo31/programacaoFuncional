import Data.Char (ord)

-- Exercício 5

l1,l2,l3,l4,l5,l6,l7,x1,x2,x3,x4,x5,x6,x7 :: [Int]
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

-- Merge Sort

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge l1 [] = l1
merge [] l2 = l2
merge (a:as) (b:bs)
    | a > b = b : (merge (a:as) bs)
    | otherwise = a : (merge as (b:bs))

merge_sort :: (Ord a) => [a] -> [a]
merge_sort [] = []
merge_sort [x] = [x]
merge_sort lst =
    let
        left = merge_sort (take ((length lst) `div` 2) lst)
        right = merge_sort (drop ((length lst) `div` 2) lst)
    in
        merge left right

-- Bucket Sort
sort_into_buckets :: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
sort_into_buckets num k m n [bucket] = if ((num * k) `div` m) <= n then [num:bucket] else [bucket]
sort_into_buckets num k m n (bucket:buckets)
    | ((num * k) `div` m) <= n = (num:bucket):buckets
    | otherwise = bucket:(sort_into_buckets num k m (n+1) buckets)

bucket_sort :: [Int] -> [Int]
bucket_sort [] = []
bucket_sort [x] = [x]
bucket_sort l1 = 
    let
        k = length l1

        m = foldr1 (max) l1

        buckets = [[] | _<-[1..k]]

        new_buckets = foldr (\x -> sort_into_buckets x k m 1) buckets l1

        sorted_buckets = map (merge_sort) new_buckets

        final_list = foldr1 (++) sorted_buckets
    in
        final_list