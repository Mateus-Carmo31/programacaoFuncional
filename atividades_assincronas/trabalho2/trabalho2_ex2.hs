-- Exercício 2

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
troca_flag :: (Ord a) => ([a], Int) -> ([a], Int)
troca_flag ([x], flag) = ([x], flag)
troca_flag ((x:y:xs), flag) = if x > y then add (troca_flag ((x:xs), 1)) y else add (troca_flag ((y:xs), flag)) x
    where
        add (l, f) e = (e:l, f) -- Função auxiliar para adicionar elemento na tupla

bubble_flag :: (Ord a) => ([a], Int) -> Int -> ([a], Int)
bubble_flag (l, flag) 0 = (l, flag)
bubble_flag (l, flag) n
    | flag == 0 = (l, flag) -- Se a flag for 0, para antecipadamente
    | otherwise = bubble_flag (troca_flag (l, 0)) (n-1)

bubble_sort1 :: (Ord a) => [a] -> [a]
bubble_sort1 [] = []
bubble_sort1 list = fst (bubble_flag (list, -1) (length list)) -- Inicializa com -1 para evitar uma parada antecipada errada

-- Variação 2
bubble_sort2 :: (Ord a) => [a] -> [a]
bubble_sort2 [] = []
bubble_sort2 lst =
    let
        troca [x] = [x]
        troca (x:y:xs) = if x > y then y : troca(x:xs) else x : troca (y:xs)

        split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)

        bubble [x] = [x]
        bubble l = (bubble parte_a_trocar) ++ ultimo_elem
            where
                lista_trocada = troca l
                (parte_a_trocar, ultimo_elem) = split lista_trocada
    in
        bubble lst

-- Variação 3
bubble_sort3 :: (Ord a) => [a] -> [a]
bubble_sort3 [] = []
bubble_sort3 l =
    let
        -- Funções auxiliares
        add (l, f) y = (y:l, f)
        split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)

        troca_flag ([x], flag) = ([x], flag)
        troca_flag ((x:y:xs), flag) = if x > y then add (troca_flag ((x:xs), 1)) y else add (troca_flag ((y:xs), flag)) x

        bubble ([x], flag) = ([x], flag)
        bubble (lst, flag)
            | n_flag == 0 = (lst, flag)
            | otherwise = (fst (bubble (parte_a_trocar, 0)) ++ ultimo_elem, 0)
            where
                (lista_trocada, n_flag) = troca_flag (lst, flag)
                (parte_a_trocar, ultimo_elem) = split lista_trocada
    in
        fst (bubble (l, -1))

-- COM CONTAGEM

-- Variação 1
troca_flag_cont :: (Ord a) => ([a], Int, Int) -> ([a], Int, Int)
troca_flag_cont ([x], flag, n) = ([x], flag, n)
troca_flag_cont ((x:y:xs), flag, n) = if x > y then add (troca_flag_cont ((x:xs), 1, n+1)) y else add (troca_flag_cont ((y:xs), flag, n+1)) x
    where
        add (l, f, c) e = (e:l, f, c) -- Função auxiliar para adicionar elemento na tupla

bubble_flag_cont :: (Ord a) => ([a], Int, Int) -> Int -> ([a], Int, Int)
bubble_flag_cont (l, flag, c) 0 = (l, flag, c)
bubble_flag_cont (l, flag, c) n
    | flag == 0 = (l, flag, c) -- Se a flag for 0, para antecipadamente
    | otherwise = bubble_flag_cont (troca_flag_cont (l, 0, c)) (n-1)

bubble_sort1_cont :: (Ord a) => [a] -> ([a], Int)
bubble_sort1_cont [] = ([], 0)
bubble_sort1_cont list = format (bubble_flag_cont (list, -1, 0) (length list)) -- Inicializa com -1 para evitar uma parada antecipada errada
    where
        format (l, _, c) = (l, c)

-- Variação 2
bubble_sort2_cont :: (Ord a) => [a] -> ([a], Int)
bubble_sort2_cont [] = ([], 0)
bubble_sort2_cont lst =
    let
        add (l, c) e = (e:l, c)

        troca ([x], c) = ([x], c)
        troca ((x:y:xs), c) = if x > y then add (troca (x:xs, c+1)) y  else add (troca (y:xs, c+1)) x

        split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)

        bubble :: (Ord a) => ([a], Int) -> ([a], Int)
        bubble ([x], c) = ([x], c)
        bubble (l, c) = (proxima_etapa ++ ultimo_elem, rec_c)
            where
                (lista_trocada, c1) = (troca (l, c))
                (parte_a_trocar, ultimo_elem) = split lista_trocada
                (proxima_etapa, rec_c) = bubble (parte_a_trocar, c1)
    in
        bubble (lst, 0)

-- Variação 3
bubble_sort3_cont :: (Ord a) => [a] -> ([a], Int)
bubble_sort3_cont [] = ([], 0)
bubble_sort3_cont l =
    let
        -- Funções auxiliares
        add (l, f, c) y = (y:l, f, c)
        split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)
        format (l, _, c) = (l,c)
        ---

        troca_flag ([x], flag, c) = ([x], flag, c)
        troca_flag ((x:y:xs), flag, c) = if x > y then add (troca_flag ((x:xs), 1, c+1)) y else add (troca_flag ((y:xs), flag, c+1)) x

        bubble ([x], flag, c) = ([x], flag, c)
        bubble (lst, flag, c)
            | n_flag == 0 = (lst, flag, c)
            | otherwise = (proxima_etapa ++ ultimo_elem, 0, rec_c)
            where
                (lista_trocada, n_flag, c1) = troca_flag (lst, flag, c)
                (parte_a_trocar, ultimo_elem) = split lista_trocada
                (proxima_etapa, _, rec_c) = bubble (parte_a_trocar, 0, c1)
    in
        format (bubble (l, -1, 0))


{- 
Em termos de funcionamento, é possível notar que a variação 3, como combinação das duas
outras, exige um número menor de comparações que as duas outras. Em alguns casos,
essa diferença é mínima, como no caso da l4 onde a variação 3 precisou de 500499
enquanto a 2 precisou de 500500. Porém, analisando-se no geral, caso não existam
outros fatores a serem considerados na implementação (como restrições de memória ou
processamento, o que poderia modificar os resultados), eu escolheria a variação 3.
-}