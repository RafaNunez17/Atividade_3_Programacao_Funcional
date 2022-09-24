{- ALUNO: Rafael Vitagliano Tannenbaum Nuñez -}

{-
1. Escreva uma função para o cálculo dos números da sequência de Fibonacci, utilizando
Haskell.
-}

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo x = fibo (x -1) + fibo (x -2)

{-
2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor
Comum (MDC) de Euclides publicado por volta do ano 300 AC. Podemos simplificar este
algoritmo dizendo que dados dois inteiros A e B, o MDC entre eles será dado pelo valor
absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva
uma função para o cálculo do MDC entre dois números inteiros positivos, usando o
algoritmo de Euclides conforme apresentado aqui, utilizando Haskell.
-}

maiorDivisorComum :: Int -> Int -> Int
maiorDivisorComum a b
  | b == 0 = a
  | a == 0 = b
  | a > b = maiorDivisorComum b (mod a b)
  | b > a = maiorDivisorComum a (mod b a)

{-
3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos
deste número. Exemplo: dado 1234 a função deverá devolver 10. Utilizando Haskell e
recursividade
-}

somaDigitos :: Int -> Int
somaDigitos 0 = 0
somaDigitos x = (mod x 10) + somaDigitos (div x 10)

{-
4. Escreva uma função que devolva a soma de todos os números menores que 10000 que
sejam múltiplos de 3 ou 5.
-}

somaNumeros :: Int
somaNumeros = sum [x | x <- [1 .. 9999], x `mod` 3 == 0 || x `mod` 5 == 0]

{-
5. Escreva uma função que, recebendo uma lista de inteiros, apresente a diferença entre a
soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade.
-}

somaQuadrados :: [Int] -> [Int]
somaQuadrados [] = []
somaQuadrados (x : y) = x ^ 2 : (somaQuadrados y)

soma :: Int -> [Int] -> Int
soma x [] = x
soma x (y : ys) = (+) y (soma x ys)

quadrado :: [Int] -> Int
quadrado x = soma 0 (somaQuadrados x)

quadradoSoma :: [Int] -> Int
quadradoSoma x = (soma 0 x) ^ 2

dif :: [Int] -> Int
dif x = (quadrado x) - (quadradoSoma x)

{-
6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma
função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números
 primos menores que um determinado inteiro dado.
-}



{-
7. Nem só de Fibonacci vivem os exemplos de recursão. Escreva uma função que devolva
todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado.
-}

lucas :: Int -> [Int]
lucas 0 = [2]
lucas 1 = [1, 2]
lucas x = head (lucas (x - 1)) + head (lucas (x - 2)) : (lucas (x - 1))

{-
8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3]
devolva [3,2,1].
-}

aoContrario :: [Int] -> [Int]
aoContrario [] = []
aoContrario (x : y) = (aoContrario y) ++ [x]

{-
9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o
produto destes valores sem usar o operador de multiplicação.
-}

somaRecursiva :: Int -> Int -> Int
somaRecursiva x 0 = 0
somaRecursiva x 1 = x
somaRecursiva x y = x + somaRecursiva x (y - 1)

{-
10. Escreva uma função chamada comprimento que receba uma lista de inteiros e devolva o
comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule
o comprimento de uma lista.
-}

comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (x : y) = comprimento y + 1

main = do
  putStrLn $ "Func. 1; entrada: 6; resultado = " ++ show (fibo 6)
  putStrLn $ "Func. 2; entrada: 12 6; resultado = " ++ show (maiorDivisorComum 12 6)
  putStrLn $ "Func. 3; entrada: 1234; resultado = " ++ show (somaDigitos 1234)
  putStrLn $ "Func. 4; entrada: sem valores de entrada; resultado = " ++ show somaNumeros
  putStrLn $ "Func. 5; entrada: [1,2,3,4,5,6]; resultado = " ++ show (dif [1, 2, 3, 4, 5, 6])
  putStrLn $ "Func. 6; não consegui fazer a questão 6."
  putStrLn $ "Func. 7; entrada: 5; resultado = " ++ show (lucas 5)
  putStrLn $ "Func. 8; entrada: [1,2,3]; resultado = " ++ show (aoContrario [1, 2, 3])
  putStrLn $ "Func. 9; entrada: 5 6; resultado = " ++ show (somaRecursiva 5 6)
  putStrLn $ "Func. 10; entrada: [3,6,8,4,2]; resultado = " ++ show (comprimento [3, 6, 8, 4, 2])
