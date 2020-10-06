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
remove_menor :: (Ord a) => (a, [a]) -> (a, [a])
remove_menor (m, [x]) = if x < m then (x, [m]) else (m, [x])
remove_menor (menor, (x:xs))
    | x < menor = add menor (remove_menor (x, xs))
    | otherwise = add x (remove_menor (menor, xs))
    where
        add a (n, l) = (n, a:l)

selection_sort2 :: (Ord a) => [a] -> [a]
selection_sort2 [] = []
selection_sort2 [x] = [x]
selection_sort2 lst =
    let
        -- Temos que usar tail para evitar conflito do menor valor com o head
        -- Problema do head duplicado
        (least, new_lst) = remove_menor (head lst, tail lst)
    
    in
        least:(selection_sort2 new_lst)

-- COM CONTAGEM
remove_menor_cont :: (Ord a) => (a, [a], Int) -> (a, [a], Int)
remove_menor_cont (m, [x], c) = if x < m then (x, [m], c+1) else (m, [x], c+1)
remove_menor_cont (menor, (x:xs), c1)
    | x < menor = add menor (remove_menor_cont (x, xs, c1+1))
    | otherwise = add x (remove_menor_cont (menor, xs,c1+1))
    where
        add a (n, l, c) = (n, a:l, c)

selection_sort2_cont :: (Ord a) => [a] -> ([a], Int)
selection_sort2_cont [] = ([], 0)
selection_sort2_cont [x] = ([x], 0)
selection_sort2_cont (x:xs) =
    let
        (least, new_lst, cont) = remove_menor_cont (x, xs, 0)

        (proxima_etapa, n_cont) = selection_sort2_cont new_lst
    in
        (least:proxima_etapa, cont + n_cont)


{- 
Em questão de procedimentos, é extremamente bom que a segunda variação não
necessite de duas passadas por iteração da ordenação, o que ajuda em casos
onde as listas são bem grandes. Mas, da maneira que eu consegui implementar
a segunda variação, ocorrem dois problemas:

a) Pela lógica de "remoção e busca do menor", que funciona guardando o
menor elemento da lista atual fora da lista e o reinsere quando algum
número ainda menor é encontrado, trocando-os, acaba que o Selection Sort,
uma vez um algoritmo de sorteamento estável, deixa de ser tal, já que não
temos garantia de onde elementos iguais serão colocados com essa busca.

b) Na minha implementação ocorreu um perda de performance, possivelmente 
causada pelo uso de 'tail'. Sinto que uma implementação desse tipo (onde a 
remoção e busca do menor elemento são simultâneas) poderia ser mais 
facilmente implementadas em um linguagem procedimental.

Por essas razões, diria que a primeira variação é melhor por, além de ser
extremamente didática e funcional, também faz bom proveito dos princípios
funcionais (o foldr)
 -}