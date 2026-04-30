{- Ejercicio 6 
a)  pasando la función smallest a funcion anónima 
-}

f6a :: Ord a => (a,a,a) -> a
f6a = \(x,y,z) -> if x <= y && x <= z then x else if y <= z then y else z

{-
9. La funcion zip3 zipea 3 listas. Dar una definicion recursiva de la funcion y otra definicion con el mismo tipo
que utilice la funcion zip. ¿Que ventajas y desventajas tiene cada definici ́on?
-}
-- zip3 recursiva
zipR3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zipR3 [] _ _ = []
zipR3 _ [] _ = []
zipR3 _ _ [] = []
zipR3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zipR3 xs ys zs

zip3Aux :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3Aux xs ys zs = map f (zip (zip xs ys) zs) where f ((x,y),z) = (x,y,z)
-- o asi map (\((x,y),z) -> (x,y,z)) (zip...)

{-
12. Dado el siguiente tipo para representar n ́umeros binarios:
type NumBin = [Bool ]
donde el valor False representa el n ́umero 0 y True el 1. Definir las siguientes operaciones tomando como convenci ́on
una representaci ́on Little-Endian (i.e. el primer elemento de las lista de d ́ıgitos es el d ́ıgito menos significativo del
n ́umero representado)
-}

type NumBin = [Bool]

-- a) suma binaria

binSum:: NumBin -> NumBin -> NumBin
binSum [] [] = []
binSum [] ys = ys
binSum xs [] = xs
binSum (x:xs) (y:ys) | not x && not y = (False: binSum xs ys)
                     | x && y = (False: binSum (binSum xs ys) [True])
                     | otherwise = (True: binSum xs ys)

-- b) producto binario

binMult:: NumBin -> NumBin -> NumBin
binMult [] [] = []
binMult [] ys = []
binMult xs [] = []
binMult (x:xs) (y:ys) | y         = binSum (x:xs) (binMult (False:(x:xs)) ys)
                      | otherwise = binMult (False:(x:xs)) ys

binCocDivDos:: NumBin -> NumBin
binCocDivDos [] = []
binCocDivDos (x:xs) | x         = binCocDivDosAux (False: xs) [False]
                    | otherwise = binCocDivDosAux (x: xs) [False]

binCocDivDosAux:: NumBin -> NumBin -> NumBin
binCocDivDosAux xs ys | binMult [False, True] ys == xs = ys
                      | otherwise =  binCocDivDosAux xs (binSum [True] ys)

binRestDivDos:: NumBin -> NumBin
binRestDivDos [] = []
binRestDivDos (x:xs) | x          = [True]
                     | otherwise = [False]

main = do
  print (binSum [True, True, True] [True]);
  print (binMult [True, True, True] [True, False, True]);
  print (binCocDivDos [True, True, True]);
  print (binCocDivDos [False, True, True, False, True]);

-- 14 El producto escalar de dos listas de enteros de igual longitud es la suma de los productos de los elementos
-- sucesivos (misma posicion) de ambas listas. Usando listas por comprensi n defina una funcion scalarproduct que
-- devuelva el producto escalar de dos listas

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]