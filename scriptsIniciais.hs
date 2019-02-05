import Data.Char
-- Funcoes nao podem comecar com letras Maiusculas

-- Quando funcoes nao tem nenhum parametro como a funcao tharcioO'Thalles chamamos ela de definicao
-- ou um nome

-- Nos podemos utilizar o comando let a = 1 por exemplo para comecar o valor de a dentro do ghci

-- Podemos utilizar ++ para adicionar listas

-- Podemos utilizar o contra operador : para adicionar instantaneamente algo a uma lista ex:
-- 5:[1,2,3] resultara em [5,1,2,3] ou 'S':"aber" resultara em Saber 


-- Podemos utilizar o !! para obter o valor do indice de uma lista começando pelo indice 0 ex:
--  ['o','l','a'] !! 1 , retornara 'l' .

-- Podemos utilizar os operadores <, >, >=, <=, ==, para poder comparar listas.

-- Podemos utilizar a funcao head para retornar o primeiro item de uma lista.

-- Podemos utilizar a funcao tail para retornar a lista com sua cabeça cortada ou
-- seja retorna toda a lista menos a sua cabeça.

-- Podemos utilizar a funcao last para retornar o ultimo item de uma lista.

-- Podemos utilizar a funcao init para retornar a lista sem o seu ultimo elemento.

-- Podemos utilizar a funcao length para retornar o tamanho de uma lista.

-- Podemos utilizar a funcao null para verificar se uma lista eh vazia, se for retorna true
-- se nao for retorna false.

-- Podemos utilizar a funcao reverse para inverter uma lista.

-- Podemos utilizar a funcao take para retornar elementos da lista ate o numero indicado ex:
-- take 2 [1,2,3,4,5,6] , retornara [1,2] .

-- Podemos utilizar a funcao drop para retornar elementos da lista do numero indicado em diante,
-- porem lembrando que o numero indicado nao sera incluido ou seja sera do numero indicado +1 para
-- frente ex: drop 3 [1,2,3,4,5,6] , retornara [4,5,6] .

-- Podemos utilizar a funcao maximum para retornar o maior valor de uma lista. se for uma lista
-- de caracteres irah retornar o maior em ordem alfabetica .

-- Podemos utilizar a funcao minimum para retornar o menor valor de uma lista. se for uma lista
-- de caracteres irah retornar o menor em ordem alfabetica .

-- Podemos utilizar a funcao sum para retornar a soma de uma lista de numeros.

-- Podemos utilizar a funcao product para retornar o produto de uma lista de numeros.

-- Podemos utilizar a funcao elem para dizer se algo faz parte de uma lista, ex:
-- elem 4 [3,4,5,6] , retornara true

--import Char


--Capitulo 1

--3)
cubo x = x * x * x

--4)
cuboq x = (quadrado x) * x

--4)
quadrado x = x * x


--5)
menor x y = if x < y then x else y

--6)
maior3 x y z = if x >= y && x >= z then x else if y > x && y > z then y else z  

--7)
maior x y = if x > y then x else y 
--7)
maior3Adapt x y z = if maior x y >= z then maior x y else z

--8)
negar x = (-x)

--9)
fun x = if x < 10 then 1 else if x < 100 then 2 else if x < 1000 then 3 else 4

--10) 
fun2 x = if x < 10 then 1 else if x < 5 then 2 else 3

--CAPITULO 2
--2)
nOr x y = if not(x || y) == True then True else False

--3)
tresDiferentes x y z = if (x /= y) && (y /= z) && (x /= z) == True then True else False 

doubleMe x = x + x

doubleUS x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else doubleMe x

doubleSmallNumber2 x = (if x > 100 then x else x*2) + 1

tharcioO'Thalles = "Humilde Demais"

imprimirAsteriscos:: Int -> String
imprimirAsteriscos n = if n > 0 then "*" ++ imprimirAsteriscos(n - 1) else ""

frase = " Relatorio de vendas "

--imprimirMeses:: Int -> Int -> String
--imprimirMeses x y = if x == 1 && x /= y then "Janeiro: " ++ "284 " ++ imprimirMeses((x+1) y) else ""

--imprimirLinhas:: String -> IO()
--imprimirLinhas n
--  | fromIntegral n == 0 = ""
--  | otherwise = "|" ++ putStr("\n")
--imprimirLinhas read n::Int = if n > 1 then putStr ("|" ++ "\n" ++ imprimirLinhas(show(n - 1))) else ""

imprimirTabela:: String -> IO()
imprimirTabela a = putStr(imprimirAsteriscos(20) ++ "\n" ++ frase ++ "\n" ++ imprimirAsteriscos(20) ++ "\n" ++ "\n" ++ "\n")

caudaLista :: [Int] -> [Int]
caudaLista[] = []
caudaLista (a:b) = b

testandoGuarda :: Int -> Int
testandoGuarda a | a == 1 = 10
  | a == 2 = 20
  | a == 3 = 30
  | otherwise = 100

somaLista :: [Int] -> Int
somaLista[] = 0
somaLista (a:b) = a + somaLista(b)

produtoLista :: [Int] -> Int
produtoLista[] = 1
produtoLista (a:b) = a * produtoLista(b)

maiorLista :: [Int] -> Int
maiorLista[a] = a
maiorLista (a:b) | a > maiorLista b = a
  | otherwise = maiorLista b

membro :: [Int] -> Int -> Bool
membro (a:b) v | a == v = True
  |b == [] = False
  |otherwise = membro b v

ultimo :: [Char] -> Char
ultimo (a:b) | b == [] = a
  |otherwise = ultimo b

membroNum :: [Int] -> Int -> Int
membroNum (a:b) c |b == [] && (a == c) = 1
  |b == [] && (a /= c) = 0
  |a == c = 1 + membroNum b c
  |otherwise  = membroNum b c

operadorE :: [Bool] -> Bool
operadorE (a:b) |b == [] = a
  |otherwise = a && operadorE b

concatenar :: [[Int]] -> [Int]
concatenar (a:b)  |b /= [] = a ++ concatenar b
  |b == [] = a

cabeca :: [Int] -> Int
cabeca (x:xs) = x 

ordenada :: [Int] -> Bool
ordenada (a:b) |b /= [] && a < cabeca b = True && ordenada b
               |b == [] = True
               | a > head b = False

paraMinuscula :: String -> String
paraMinuscula(a:b) |b /= [] = [(chr(ord a + 32))] ++ paraMinuscula b
                   |b == [] = [(chr(ord a + 32))]

substituir :: String -> Char -> Char -> String
substituir (a:b) c d
  |[a] /= [c] && b /= [] = [a] ++ substituir b c d 
  |[a] == [c] && b /= [] = [d] ++ substituir b c d
  |[a] /= [c] && b == [] = [a]
  |[a] == [c] && b == [] = [d]

-- substitui soh uma vez
substituir2 :: String -> Char -> Char -> String
substituir2 (a:b) c d
  |[a] /= [c] && b /= [] = [a] ++ substituir2 b c d 
  |[a] == [c] && b /= [] = [d] ++ b
  |[a] /= [c] && b == [] = [a]
  |[a] == [c] && b == [] = [d]

divisores :: Int -> [Int]
divisores a
  |a == 1 = [a]
  |mod a a == 0 = [a] ++ divisores (a-1)

divisores2 :: Int -> Int -> [Int]
divisores2 a b
  |b == 2 = [1]
  |mod a (cabeca(divisores (b - 1))) /= 0 = divisores2 a (b-1)
  |mod a (cabeca(divisores (b - 1))) == 0 = [b - 1] ++ divisores2 a (b-1)

divisores3 :: Int -> Int -> [Int]
divisores3 a b 
  |b == 1 = [b]
  |mod a b == 0 = [b] ++ divisores3 a (b - 1)
  |mod a b /= 0 = divisores3 a (b - 1)

somaListas :: [[Int]] -> Int
  somaListas[] = 0
  somaListas(a:b) = somaValores a + somaListas b

somaValores :: [Int] -> Int
  somaValores[] = 0
  somaValores (a:b) = a + somaValores b
  