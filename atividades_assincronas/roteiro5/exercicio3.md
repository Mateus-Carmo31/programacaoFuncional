# Exercício 3

### 1) $(\lambda x.\ 2x + 1)\ 3$

$$
(\lambda x.\ 2x + 1)\ 3 \Longrightarrow
(2(3) + 1) \Longrightarrow
7
$$

Resposta: $7$
___
### 2) $(\lambda xy.\ x-y)\ 5\ 7$

$$
(\lambda xy.\ x-y)\ 5\ 7 \Longrightarrow
((5)-(7)) \Longrightarrow
-2
$$

Resposta: $-2$
___
### 3) $(\lambda yx.\ x-y)\ 5\ 7$

$$
(\lambda yx.\ x-y)\ 5\ 7 \Longrightarrow
((7)-(5)) \Longrightarrow
2
$$

Resposta: $2$
___
### 4) $(\lambda xy.\ x-y)\ (\lambda z.\ z/2)$

$$
(\lambda xy.\ x-y)\ (\lambda z.\ z/2)
$$

Resposta: Não é possível reduzir mais do que isso, logo a resposta é $(\lambda xy.\ x-y)\ (\lambda z.\ z/2)$.

___
### 5) $(\lambda xy.\ x-y)\ ((\lambda z.\ z/2)\ 6)\ 1$

$$
(\lambda xy.\ x-y)\ ((\lambda z.\ z/2)\ 6)\ 1 \Longrightarrow
(\lambda xy.\ x-y)\ ((6)/2)\ 1 \Longrightarrow\\
(\lambda xy.\ x-y)\ 3\ 1 \Longrightarrow
((3) - (1)) \Longrightarrow
2
$$

Resposta: $2$

___
### 6) $(\lambda x.\ \lambda y.\ - x\ y)\ 9\ 4$

$$
(\lambda x.\ \lambda y.\ - x\ y)\ 9\ 4 \Longrightarrow
(- 9\ 4) \Longrightarrow
5
$$

Resposta: $5$

___
### 7) $(\lambda x.\ xx)\ (\lambda y.\ y)$

$$
(\lambda x.\ xx)\ (\lambda y.\ y) \Longrightarrow
(\lambda y.\ y)(\lambda y.\ y) \Longrightarrow
(\lambda y.\ y)
$$

Resposta: $(\lambda y.\ y)$