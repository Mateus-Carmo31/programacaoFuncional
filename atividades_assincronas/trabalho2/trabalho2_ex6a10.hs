-- TIPOS ALGÉBRICOS

-- Exercício 6

data Expr a = Val a
            | Add (Expr a) (Expr a)
            | Sub (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Pot (Expr a) (Expr a)

avalia :: (Floating a, Integral a) => Expr a -> a
avalia (Val x) = x
avalia (Add e1 e2) = (avalia e1) + (avalia e2)
avalia (Sub e1 e2) = (avalia e1) - (avalia e2)
avalia (Mul e1 e2) = (avalia e1) * (avalia e2)
avalia (Pot e1 e2) = (avalia e1) ** (avalia e2)

exp1, exp2 :: Num a => Expr a
exp1 = Mul (Add (Val 3) (Val 12)) (Pot (Sub (Val 15) (Val 5)) (Mul (Val 1) (Val 3))) -- = 15000
exp2 = Mul (Add (Sub (Add (Val 6) (Val 8)) (Val 5)) (Val 1)) (Add (Val 2) (Pot (Val 6) (Val 2))) -- = 380

-- Exercício 7

-- Letra A, B e C
data Hora = AM Int Int | PM Int Int
    deriving (Eq, Ord, Show)

-- horasDecorridas :: Hora -> Int
-- horasDecorridas (AM hr _) = hr
-- horasDecorridas (PM hr _) = hr + 12

-- minutosDecorridos :: Hora -> Int
-- minutosDecorridos (AM hr min) = hr * 60 + min
-- minutosDecorridos (PM hr min) = (hr + 12) * 60 + min

-- segundosDecorridos :: Hora -> Int
-- segundosDecorridos (AM hr min) = (hr * 60 + min) * 60
-- segundosDecorridos (PM hr min) = ((hr + 12) * 60 + min) * 60

horasDecorridas :: Hora -> Int
horasDecorridas (AM hr min) = if not(hr < 0 || hr > 11 || min < 0 || min > 59) then hr else undefined
horasDecorridas (PM hr min) = if not(hr < 0 || hr > 11 || min < 0 || min > 59) then hr + 12 else undefined

minutosDecorridos :: Hora -> Int
minutosDecorridos (AM hr min) = if not(hr < 0 || hr > 11 || min < 0 || min > 59) then hr * 60 + min else undefined
minutosDecorridos (PM hr min) = if not(hr < 0 || hr > 11 || min < 0 || min > 59) then (hr + 12) * 60 + min else undefined

segundosDecorridos :: Hora -> Int
segundosDecorridos (AM hr min) = if not(hr < 0 || hr > 11 || min < 0 || min > 59) then (hr * 60 + min) * 60 else undefined
segundosDecorridos (PM hr min) = if not(hr < 0 || hr > 11 || min < 0 || min > 59) then ((hr + 12) * 60 + min) * 60 else undefined

-- Exercício 8

type Data = (Int, Int, Int)

precede :: Data -> Data -> Bool
precede (d1, m1, y1) (d2, m2, y2)
 | y1 > y2 = False
 | y1 == y2 && m1 > m2 = False
 | y1 == y2 && m1 == m2 && d1 > d2 = False
 | otherwise = True

data Contato = Nome String | Fone String
              deriving (Eq, Show)
data Mensagem = Msg Contato String Data Hora String
              deriving (Show)

msgs :: [Mensagem]
msgs = [
    (Msg (Nome "A. Costa") "A apresentacao de 13h foi cancelada" (13, 09, 2020) (AM 10 30) "WhatsApp"),
    (Msg (Fone "3232-3232") "Reuniao 14h" (14, 09, 2020) (AM 08 50) "WhatsApp"),
    (Msg (Nome "A. Silva") "Banco de Talentos da USP" (14, 09, 2020) (PM 06 57) "LinkedIn"),
    (Msg (Nome "D. Costa") "Aula de hoje" (14, 09, 2020) (AM 11 10) "Facebook"),
    (Msg (Nome "A. Costa") "Esquema de amanha" (14, 09, 2020) (PM 1 10) "Whatsapp"),
    (Msg (Nome "P. Ribeiro") "Solicitacao de amizade" (13, 09, 2020) (AM 11 10) "LinkedIn"),
    (Msg (Nome "P. Ribeiro") "Slides" (14, 09, 2020) (AM 8 20) "Gmail"),
    (Msg (Nome "P. Ribeiro") "Correcao" (13, 09, 2020) (PM 11 10) "Gmail"),
    (Msg (Fone "69-69-69") "Veja o link aaa.bbb.com" (13, 09, 2020) (AM 11 10) "Facebook"),
    (Msg (Fone "69-69-69") "Veja o link aaa.bbb.com" (14, 09, 2020) (PM 4 21) "Facebook"),
    (Msg (Nome "P. Ribeiro") "Foto" (14, 09, 2020) (PM 0 10) "Whatsapp"),
    (Msg (Nome "A. Costa") "Foto" (13, 09, 2020) (PM 11 10) "Facebook"),
    (Msg (Nome "A. Costa") "Oportunidade de Emprego" (14, 09, 2020) (AM 11 10) "LinkedIn"),
    (Msg (Nome "A. Costa") "Oportunidade de Emprego" (14, 09, 2020) (PM 5 10) "LinkedIn"),
    (Msg (Nome "A. Zimmer") "Solicitacao de Amizade" (13, 09, 2020) (AM 8 10) "Facebook"),
    (Msg (Nome "A. Zimmer") "E ai parca" (14, 09, 2020) (PM 7 5) "Facebook"),
    (Msg (Nome "A. Zimmer") "Queria te perguntar..." (14,09,2020) (PM 7 6) "Facebook"),
    (Msg (Fone "000-000-0000") "paosdiapksn" (14,9,2020) (AM 0 1) "IRC"),
    (Msg (Fone "000-000-0000") "asdasdads" (14,09,2020) (AM 0 0) "IRC"),
    (Msg (Nome "A. Zimmer") "Como vc vai fazer com o Jonas?" (14,09,2020) (PM 7 6) "Facebook"),
    (Msg (Nome "A. Zimmer") "Tem algo estranho rolando com ele. Recebi umas 20 mensagens dele so ontem." (14,09,2020) (PM 7 7) "Facebook"),
    (Msg (Nome "J. da Silva") "adsasdsdad" (14,09,2020) (AM 0 0) "WhatsApp"),
    (Msg (Nome "J. da Silva") "adsasdsdad" (14,09,2020) (AM 0 1) "WhatsApp"),
    (Msg (Nome "J. da Silva") "adsasdsdad" (14,09,2020) (AM 0 2) "WhatsApp"),
    (Msg (Nome "J. da Silva") "adsasdsdad" (14,09,2020) (AM 0 3) "WhatsApp"),
    (Msg (Nome "J. da Silva") "adsasdsdad" (14,09,2020) (AM 0 4) "WhatsApp"),
    (Msg (Fone "998896660") "Chamada perdida" (13,09,2020) (AM 10 25) "WhatsApp"),
    (Msg (Fone "998896660") "asdasdads" (13,09,2020) (AM 10 25) "WhatsApp"),
    (Msg (Nome "M. Carmo") "Tudo q eu fiz foi remover o python cara, como q eu ia saber q ia ferrar o SO?!" (13,09,2020) (PM 0 5) "WhatsApp"),
    (Msg (Nome "M. Carmo") "Mano, o servidor caiu de novo." (13,09,2020) (PM 0 4) "WhatsApp")]

ordenar_por_contato :: [Mensagem] -> [Mensagem]
ordenar_por_contato [] = []
ordenar_por_contato list = bubble list (length list)

bubble :: [Mensagem] -> Int -> [Mensagem]
bubble list 0 = list
bubble list n = bubble (troca list) (n-1)

troca :: [Mensagem] -> [Mensagem]
troca [x] = [x]
troca (msg1:msg2:xs)
    | comp msg1 msg2 = msg2:troca (msg1:xs)
    | otherwise = msg1:troca (msg2:xs)
    where
        comp (Msg (Nome _) _ _ _ _) (Msg (Fone _) _ _ _ _) = True -- Ocorre troca, fone vem primeiro
        comp (Msg (Fone _) _ _ _ _) (Msg (Nome _) _ _ _ _) = False -- Não ocorre troca
        comp (Msg (Nome n) _ _ _ _) (Msg (Nome m) _ _ _ _) = n > m
        comp (Msg (Fone n) _ _ _ _) (Msg (Fone m) _ _ _ _) = n > m

-- Vê se a mensagem 1 precede a 2
msg_precede :: Mensagem -> Mensagem -> Bool
msg_precede (Msg _ _ d1 h1 _) (Msg _ _ d2 h2 _)
    | d1 == d2 = (minutosDecorridos h1) < (minutosDecorridos h2)
    | otherwise = precede d1 d2

-- Ordena pelas mais recentes
ordenar_por_data_hora :: [Mensagem] -> [Mensagem]
ordenar_por_data_hora [] = []
ordenar_por_data_hora (piv:xs) = (ordenar_por_data_hora [x | x<-xs, (msg_precede x piv) == False]) 
                                 ++ [piv] ++
                                 (ordenar_por_data_hora [x | x<-xs, (msg_precede x piv) == True])

ultimas_duas_msgs :: Contato -> [Mensagem] -> [Mensagem]
ultimas_duas_msgs contact msgs = take 2 [(Msg c m d h a) | (Msg c m d h a)<-sorted_msgs, c == contact]
    where
        sorted_msgs = ordenar_por_data_hora msgs

-- Exercício 9
data ArvBinInt = Nulo
                | No Int ArvBinInt ArvBinInt

arvDados :: ArvBinInt 
arvDados = No 4 (No 2 Nulo Nulo) (No 10 (No 5 Nulo Nulo) (No 15 Nulo Nulo))

internos :: ArvBinInt -> [Int]
internos Nulo = []
internos (No num Nulo Nulo) = []
internos (No num left right) = (internos left) ++ [num] ++ (internos right)

somaNos :: ArvBinInt -> Int
somaNos Nulo = 0
somaNos (No num left right) = (somaNos left) + num + (somaNos right)

-- Busca em árvore binária
pertence :: Int -> ArvBinInt -> Bool
pertence _ Nulo = False
pertence n (No num left right)
    | n == num = True
    | n < num = pertence n left
    | otherwise = pertence n right

-- Exercício 10
data ArvBinEA a = Vazia
                | Folha a
                | NoEA (Char, ArvBinEA a, ArvBinEA a)
                deriving (Show)

arvEA :: ArvBinEA Float
arvEA = NoEA ('+', NoEA ('*', Folha 10, Folha 5), Folha 7)

avalia_arv :: Floating a => ArvBinEA a -> a
avalia_arv Vazia = 0
avalia_arv (Folha num) = num
avalia_arv (NoEA (op, left, right))
    | op == '+' = (avalia_arv left) + (avalia_arv right)
    | op == '-' = (avalia_arv left) - (avalia_arv right)
    | op == '*' = (avalia_arv left) * (avalia_arv right)
    | op == '/' = (avalia_arv left) / (avalia_arv right)
    | op == '^' = (avalia_arv left) ** (avalia_arv right)
    | otherwise = undefined