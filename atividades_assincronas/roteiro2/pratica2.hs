-- Exercício 1
double :: Float -> Float
double x = x * 2

quad :: Float -> Float
quad x = double (double x)

hip :: Float -> Float -> Float
hip catA catB = sqrt (catA^2 + catB^2)

dist :: Float -> Float -> Float -> Float -> Float
dist xA yA xB yB = sqrt ((xB-xA)^2 + (yB-yA)^2)

-- Exercício 3
conversao :: Float -> (Float, Float, Float)
conversao real = (real, real * 3.96, real * 4.45)

-- Exercício 4
bissexto :: Int -> Bool
bissexto x
 | mod x 4 /= 0   = False
 | mod x 100 /= 0 = True
 | mod x 400 /= 0 = False
 | otherwise      = True

-- Uma outra maneira de realizar a função bissexto, sem guardas
bissextoOutro :: Int -> Bool
bissextoOutro x = ((mod x 4 == 0) && not(mod x 100 == 0)) || (mod x 4 == 0 && mod x 100 == 0 && mod x 400 == 0)

-- Exercício 5
type Data = (Int, Int, Int)
bissexto2 :: Data -> Bool
bissexto2 (d, m, y) = bissexto y

-- Exercício 6 (decidi considerar anos negativos como A.C.)
valida :: Data -> Bool
valida (d, m, y)
 | d < 1 || d > 31 || m < 1 || m > 12 = False
 | m == 2 = (if (bissexto y) then not(d > 29) else not(d > 28))
 | (m == 4 || m == 6 || m == 9 || m == 11) && d > 30 = False
 | otherwise = True

-- Exercício 7
precede :: Data -> Data -> Bool
precede (d1, m1, y1) (d2, m2, y2)
 | not(valida (d1, m1, y1) && valida (d2,m2,y2)) = False
 | y1 > y2 = False
 | y1 == y2 && m1 > m2 = False
 | y1 == y2 && m1 == m2 && d1 > d2 = False
 | otherwise = True

-- Exercício 8
type Livro = (String, String, String, String, Int)
type Aluno = (String, String, String, String)
type Emprestimo = (String, String, Data, Data, String)

-- Exercício 9
checar_status :: Emprestimo -> Data -> Bool
checar_status (c_livro, c_aluno, d_emprestimo, d_devol, status) data_hj = precede data_hj d_devol