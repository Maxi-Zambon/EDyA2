{- Ejercicio 6 
a)  pasando la función smallest a funcion anónima 
-}
f :: Ord a => (a,a,a) -> a
f = \(x,y,z) -> if x <= y && x <= z then x else if y <= z then y else z

