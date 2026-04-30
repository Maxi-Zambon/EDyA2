{-
1. El modelo de color RGB es un modelo aditivo que tiene al rojo, verde y azul como colores primarios. Cualquier
otro color se expresa en terminos de los porcentajes de cada uno estos tres colores que es necesario combinar
en forma aditiva para obtenerlo. Dichas proporciones caracterizan a cada color de manera biunivoca, por lo que
usualmente se utilizan estos valores como representaci ́on de un color.
Definir un tipo Color en este modelo y una funci ́on mezclar que permita obtener el promedio componente a
componente entre dos colores.
-}

data Color = RGB Int Int Int deriving Show

mezclar :: Color -> Color -> Color
mezclar (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (div (r1+r2) 2) (div (g1+g2) 2) (div (b1+b2) 2)     


{-
2. Consideremos un editor de lineas simple. Supongamos que una Linea es una secuencia de caracteres c1, c2, . . . , cn
junto con una posicion p, siendo 0 6p 6n, llamada cursor (consideraremos al cursor a la derecha de un caracter
que sera borrado o insertado, es decir como el cursor de la mayoria de los editores). Se requieren las siguientes
operaciones sobre lineas:
vacia :: Linea
moverIzq :: Linea → Linea
moverDer :: Linea → Linea
moverIni :: Linea → Linea
moverFin :: Linea → Linea
insertar :: Char → Linea → Linea
borrar :: Linea → Linea
La descripcion informal es la siguiente: (1) la constante vacia denota la linea vacia, (2) la operacion moverIzq
mueve el cursor una posicion a la izquierda (siempre que ello sea posible), (3) analogamente para moverDer , (4)
moverIni mueve el cursor al comienzo de la linea, (5) moverFin mueve el cursor al final de la linea, (6) la operacion
borrar elimina el caracterer que se encuentra a la izquierda del cursor, (7) insertar agrega un caracter en el lugar
donde se encontraba el cursor, dejando al caracter insertado a su izquierda.
Definir un tipo de datos Linea e implementar las operaciones dadas.
-}

data Linea = TC String Int deriving Show

vacia :: Linea
vacia = TC [] 0

moverIzq :: Linea -> Linea
moverIzq (TC s cursor)  | cursor > 0 = TC s (cursor - 1)
                        | otherwise = TC s cursor

moverDer :: Linea -> Linea
moverDer (TC s cursor)  | cursor >= length s = TC s cursor 
                        | otherwise = TC s (cursor + 1)

moverIni :: Linea -> Linea
moverIni (TC s cursor) = TC s 0

moverFin :: Linea -> Linea
moverFin (TC s cursor) = TC s (length s)

insertar :: Char -> Linea->  Linea
insertar c (TC s cursor)    | s == [] = TC [c] 1       
                            | cursor == 0 = TC (c : s) (cursor + 1)
                            | otherwise = TC (head s : resto) (cursor + 1) where TC resto p = insertar c (TC (tail s) (cursor - 1))
 
borrar :: Linea -> Linea
borrar (TC s cursor)    | s == [] || cursor == 0 = TC s cursor 
                        | cursor == 1 = TC (tail s) 0
                        | otherwise = TC  (head s : resto) (cursor - 1) where TC resto p = borrar (TC (tail s) (cursor - 1))

{-


-}






main :: IO ()
main = do
    print (insertar 'a' (TC "Hola" 0))
    print (insertar 'a' (TC "Hola" 2))
    print (insertar 'a' (TC "Hola" 4))
    print (insertar 'a' (TC "Hola" 6))
    print (borrar (TC "Hola" 0))
    print (borrar (TC "Hola" 2))
    print (borrar (TC "Hola" 4))
    print (borrar (TC "Hola" 6))