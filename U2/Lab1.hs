module Lab01 where

import Data.List
import Data.Char (ord, isLetter)

{-
1) Corregir los siguientes programas de modo que sean aceptados por GHCi.
-}

-- a) Se usan espacios y no identaciones
myNot b = case b of
    True -> False
    False -> True

-- b) Se cambió 'in' por 'myInit' porque 'in' es palabra reservada.
myInit :: [a] -> [a]
myInit [x] = []
myInit (x:xs) = x : myInit xs
myInit [] = error "empty list"

-- c) Se usa mayuscula para los tipos, no para las funciones.
myLength []        =  0
myLength (_:l)     =  1 + myLength l

-- d) Error en cons, toma un elemento a derecha y una lista a izquierda
list123 = (1: (2: (3: [])))


-- e) Estoy tratando de definir un operador, pero no defini como asocia,
-- por ende la expresion del cons no se sabe como evaluar
[]     ++! ys = ys
(x:xs) ++! ys = x : (xs ++! ys)

-- f) Para definir una seccion necesito parentesis y necesito parentesis para
-- la cola.
addToTail x xs = map (+x) (tail xs)

-- g) Para el operador composicion debo indicar entre parentesis las funciones
-- a componet
listmin xs = (head . sort) xs

-- h) Tenemos que (smap f):: [a] -> [b], entonces no se lo podemos dar
-- a la funcion smap, pues la funcion toma una funcion a->b
smap:: (a -> b) -> [a] -> [b]
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs

{-
2. Definir las siguientes funciones y determinar su tipo:

a) five, que dado cualquier valor, devuelve 5

b) apply, que toma una función y un valor, y devuelve el resultado de
aplicar la función al valor dado

c) ident, la función identidad

d) first, que toma un par ordenado, y devuelve su primera componente

e) derive, que aproxima la derivada de una función dada en un punto dado

f) sign, la función signo

g) vabs, la función valor absoluto (usando sign y sin usarla)

h) pot, que toma un entero y un número, y devuelve el resultado de
elevar el segundo a la potencia dada por el primero

i) xor, el operador de disyunción exclusiva

j) max3, que toma tres números enteros y devuelve el máximo entre llos

k) swap, que toma un par y devuelve el par con sus componentes invertidas
-}

five:: a -> Int
five _ = 5

apply:: (a -> b) -> a -> b
apply f x = f x

id:: a -> a
id x = x

first:: (a, b) -> a
first (x, y) = x

-- h debe ser pequeño pero no tanto con 10^-8 va.
derive:: Fractional a => (a -> a) -> a -> a ->  a
derive f x h = (f (x + h) - f (x - h)) / (2 * h)

sign:: (Num a, Ord a) => a -> Int
sign x | x < 0 = -1
       | x == 0 = 0
       | x > 0 = 1

vabs:: (Num a, Ord a) => a -> a
vabs x | x < 0 = -x
       | otherwise = x

vabsSign:: (Num a, Ord a) => a -> a
vabsSign x = if sign x == -1 then -x else x

pot:: Num a => a -> Int -> a
pot x 0 = 1
pot x n = x * pot x (n - 1)

xor:: Bool -> Bool -> Bool
xor b1 b2 = b1 /= b2

max3:: (Num a, Ord a) => a -> a -> a -> a
max3 x y z | x>=y && x>=z = x
           | y>=x && y>=z = y
           | z>=x && z>=y = z

swap:: (a, b) -> (b, a)
swap (x, y) = (y, x)

{- 
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}

bisiesto:: Int -> Bool
bisiesto anio = (mod anio 4) == 0 && ((mod anio 100) /= 0 || (mod anio 400) == 0)

{-
4)

Defina un operador infijo *$ que implemente la multiplicación de un
vector por un escalar. Representaremos a los vectores mediante listas
de Haskell. Así, dada una lista ns y un número n, el valor ns *$ n
debe ser igual a la lista ns con todos sus elementos multiplicados por
n. Por ejemplo,

[ 2, 3 ] *$ 5 == [ 10 , 15 ].

El operador *$ debe definirse de manera que la siguiente
expresión sea válida:

-}

infixl 8 *$

(*$):: Num a => [a] -> a -> [a]
(*$) [] _ = []
(*$) (x:xs) n = (x*n: xs *$ n)

v = [1, 2, 3] *$ 2 *$ 4

{-
5) Definir mediante recursión explícita
las siguientes funciones y escribir su tipo más general:

a) 'suma', que suma todos los elementos de una lista de números

b) 'alguno', que devuelve True si algún elemento de una
lista de valores booleanos es True, y False en caso
contrario

c) 'todos', que devuelve True si todos los elementos de
una lista de valores booleanos son True, y False en caso
contrario

d) 'codes', que dada una lista de caracteres, devuelve la
lista de sus ordinales

e) 'restos', que calcula la lista de los restos de la
división de los elementos de una lista de números dada por otro
número dado

f) 'cuadrados', que dada una lista de números, devuelva la
lista de sus cuadrados

g) 'longitudes', que dada una lista de listas, devuelve la
lista de sus longitudes

h) 'orden', que dada una lista de pares de números, devuelve
la lista de aquellos pares en los que la primera componente es
menor que el triple de la segunda

i) 'pares', que dada una lista de enteros, devuelve la lista
de los elementos pares

j) 'letras', que dada una lista de caracteres, devuelve la
lista de aquellos que son letras (minúsculas o mayúsculas)

k) 'masDe', que dada una lista de listas 'xss' y un
número 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' 
-}

suma:: Num a => [a] -> a
suma [] = 0
suma (x:xs) = x + suma xs

alguno:: [Bool] -> Bool
alguno [] = False
alguno (x:xs) = x || alguno xs

todos:: [Bool] -> Bool
todos [] = False
todos [x] = x
todos (x:xs) = x && todos xs

codes :: [Char] -> [Int]
codes [] = []
codes (x:xs) = ord x : codes xs

restos:: [Int] -> Int -> [Int]
restos [] _ = []
restos (x:xs) y = (mod x y: restos xs y)

cuadrados :: Num a => [a] -> [a]
cuadrados [] = []
cuadrados (x:xs) = (x * x) : cuadrados xs

longitudes:: [[a]] -> [Int]
longitudes [] = []
longitudes (x:xs) = (length x: longitudes xs)

orden :: (Ord a, Num a) => [(a, a)] -> [(a, a)]
orden [] = []
orden ((x, y):xs) | x < 3 * y = (x, y) : orden xs
                  | otherwise = orden xs

pares :: Integral a => [a] -> [a]
pares [] = []
pares (x:xs) | mod x 2 == 0 = x : pares xs
             | otherwise    = pares xs

letras :: [Char] -> [Char]
letras [] = []
letras (x:xs) | isLetter x = x : letras xs
              | otherwise  = letras xs

masDe :: [[a]] -> Int -> [[a]]
masDe [] _ = []
masDe (x:xs) n | length x > n = x : masDe xs n
               | otherwise      = masDe xs n
