
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
3. Dado el tipo de datos
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a
a) Implementar las operaciones de este tipo algebraico teniendo en cuenta que:
    Las funciones de acceso son headCL, tailCL, isEmptyCL, isCUnit.
    headCL y tailCL no estan definidos para una lista vacia.
    headCL toma una CList y devuelve el primer elemento de la misma (el de mas a la izquierda).
    tailCL toma una CList y devuelve la misma sin el primer elemento.
    isEmptyCL aplicado a una CList devuelve True si la CList es vacia (EmptyCL) o False en caso contrario.
    isCUnit aplicado a una CList devuelve True sii la CList tiene un solo elemento (CUnit a) o False en caso
    contrario.
-}

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

headCL :: CList a -> a
headCL (CUnit x) = x
headCL (Consnoc x l y) = x

tailCL :: CList a -> CList a
tailCL (CUnit x) = EmptyCL
tailCL (Consnoc x EmptyCL y) = CUnit y
tailCL (Consnoc x l y) = Consnoc (headCL l) (tailCL l) y

-- Resolucion en pizzarron de la catedra, sin usar headCL
tailCL2 :: CList a -> CList a
tailCL2 (CUnit x) = EmptyCL
tailCL2 (Consnoc x l y) = appendElem l y 

appendElem :: CList a -> a -> CList a
appendElem EmptyCL x = CUnit x
appendElem (CUnit y) x = Consnoc y EmptyCL x
appendElem (Consnoc a l b) x = Consnoc a (appendElem l b) x

-----------------------------------

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnit :: CList a -> Bool
isCUnit (CUnit x) = True
isCUnit _ = False

-------------------------------------------------------------
-- b) Definir una funcion reverseCL que toma una CList y devuelve su inversa.

reverseCL :: CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x EmptyCL y) = Consnoc y EmptyCL x  -- es redundante pero no esta mal, se podria sacar 
reverseCL (Consnoc x l y) = Consnoc y (reverseCL l) x

-- c) Definir una funcion inits que toma una CList y devuelve una CList con todos los posibles inicios de la CList.
-- inits [] -> [[]], initis [1] -> [[],[1]], inits [1,2] -> [[],[1],[1,2]] , inits [1,2,3] -> [[],[1],[1,2],[1,2,3]] 
-- inits [1,2,3,4](=lista) -> [[],[1],[1,2],[1,2,3],[1,2,3,4]] y m = [[1],[1,2],[1,2,3]] = [[],[2],[2,3]] + 1 = inits [2,3] + 1
-- lista = x l y                                                                           inits l          x           l  +  x

inits :: CList a -> CList (CList a)
inits EmptyCL = CUnit EmptyCL
inits (CUnit x) = Consnoc EmptyCL EmptyCL (CUnit x)
inits lista@(Consnoc x l y) = Consnoc EmptyCL m lista where m = mapPrepend (inits l) x 

                                            --  where m = tailCL(inits (consCL x l))
-- en la segunda idea agarro la cola (pues el [] no me interesa porque ya lo tengo) de los inicialces de la lista 
-- sin el ultimo elemento (y) pues este solo aparece en la ultima sublista del resultado de inits (cuando pongo la lista completa) 
-- y este caso ya lo tengo cubierto en el consnoc con lista (lo que recibe la funcion). 
-- Luego uso la funcion que agrega x a l, pues tengo que reconstruir la lista antes de hacerle inits


-- Recibe una lista de listas y le agrega al inicio de cada lista (elemento de la lista de listas) el elemento que recibe
mapPrepend :: CList (CList a) -> a -> CList (CList a)
mapPrepend EmptyCL x = EmptyCL 
mapPrepend (CUnit l) x = CUnit (consCL x l)
mapPrepend (Consnoc l1 ls l2) x = Consnoc (consCL x l1) (mapPrepend ls x) (consCL x l2)

-- Recibe un elemento y una lista y añande el elemento al inicio de la lista
consCL :: a -> CList a -> CList a
consCL x EmptyCL = CUnit x
consCL x (CUnit a) = Consnoc x EmptyCL a
consCL x (Consnoc a l b) = Consnoc x (consCL a l) b

-- d) Definir una funcion lasts que toma una CList y devuelve una CList con todas las posibles terminaciones de la CList
lasts :: CList a -> CList (CList a)
lasts EmptyCL = CUnit EmptyCL
lasts (CUnit x) = Consnoc (CUnit x) EmptyCL EmptyCL
lasts lista@(Consnoc x l y) = Consnoc lista (mapAppend (lasts l) y ) EmptyCL

mapAppend :: CList (CList a) -> a -> CList (CList a)
mapAppend EmptyCL x = EmptyCL
mapAppend (CUnit l) x = CUnit (consEndCL l x)
mapAppend (Consnoc l1 ls l2) x = Consnoc (consEndCL l1 x) (mapAppend ls x) (consEndCL l2 x)

consEndCL ::  CList a -> a -> CList a
consEndCL EmptyCL x = CUnit x
consEndCL (CUnit a) x = Consnoc a EmptyCL x
consEndCL (Consnoc a l y) x = Consnoc a (consEndCL l y) x

-- e)  Definir una funcion concatCL que toma una CList de CList y devuelve la CList con todas ellas concatenadas

concatCL :: CList (CList a) -> CList a
concatCL EmptyCL = EmptyCL
concatCL (CUnit l) = l
concatCL (Consnoc l1 EmptyCL l2) = concatLists l1 l2 
concatCL (Consnoc l1 (CUnit l) l2) = concatLists (concatLists l1 l) l2
concatCL (Consnoc l1 ls l2) = concatLists (concatLists l1 (headCL ls)) (concatCL (consEndCL (tailCL ls) l2))

concatLists :: CList a -> CList a -> CList a
concatLists EmptyCL EmptyCL = EmptyCL
concatLists EmptyCL a = a
concatLists x EmptyCL = x
concatLists (CUnit x) (CUnit a) = Consnoc x EmptyCL a
concatLists (CUnit x) (Consnoc a ls2 b) = Consnoc x (consCL a ls2) b
concatLists (Consnoc x ls1 y) (CUnit a) = Consnoc x (consEndCL ls1 y) a
concatLists (Consnoc x ls1 y) (Consnoc a ls2 b) = Consnoc x (concatLists (consEndCL ls1 y)(consCL a ls2))  b  

-- 4) Defina un evaluador eval :: Exp → Int para el siguiente tipo algebraico:

data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp deriving Show

eval :: Exp -> Int 
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Prod x y) = eval x * eval y
eval (Div x y) = div (eval x) (eval y) 

{-
5) La notacion polaca inversa o RPN (del ingles Reverse Polish Notation) es una manera alternativa de escribir
expresiones matematicas, en la cual los operadores se escriben luego de los operandos, es decir, usando notacion
posfija. Por ejemplo, la suma de los enteros 3 y 5 que con notacion infija notamos 3 + 5, en RPN se escribe 3 5 +
Para evaluar una expresion escrita en RPN, podemos usar un stack o pila. Recorremos la expresion de izquierda
a derecha. Cada vez que encontramos un numero, lo apilamos. Cada vez que encontramos un operador, retiramos
los dos numeros que estan en la cima de la pila, le aplicamos el operador y apilamos el resultado. Si la expresion
esta bien formada, al alcanzar el final de la misma, debemos tener un unico numero en la pila, que representa el
resultado de la expresion.
-}

-- a) Defina una funcion parseRPN :: String → Exp que, dado un string que representa una expresion escrita en
-- RPN, construya un elemento del tipo Exp presentado en el ejercicio 4 correspondiente a la expresion dada. Por
-- ejemplo:
-- parseRPN “8 5 3 − 3 ∗ +” = Add (Lit 8) (Prod (Sub (Lit 5) (Lit 3)) (Lit 3)) 

-- Definicion de la pila y sus funciones
data Stack a = EmptyS | ConsS a (Stack a) deriving Show

push :: a -> Stack a -> Stack a
push x EmptyS = ConsS x EmptyS
push x stack = ConsS x stack

pop :: Stack a -> Stack a
pop (ConsS x stack) = stack

top :: Stack a -> a
top (ConsS x stack) = x

-------------------------------------------- 

-- Llamo a la funcion auxiliar que hace toda la logica con el string separado en strings por espacio y la pila inicilizada vaica
parseRPN :: String -> Exp 
parseRPN string = parseRPNAux (words string) EmptyS 

-- Iba a hacer que se puedan pasar numeros de mas de una cifra en la cuenta pero me dio paja
parseRPNAux :: [String] -> Stack Exp -> Exp
parseRPNAux [] stack = top stack
parseRPNAux (x:xs) stack    | isNumeric (head x) = parseRPNAux xs (push (Lit (read x) ) stack) 
                            | isOperator (head x) = 
                            let op2 = top stack
                                middleStack = pop stack
                                op1 = top middleStack
                                newStack = pop middleStack
                            in parseRPNAux xs (push (toExp x op1 op2) newStack )

toExp :: String -> Exp -> Exp -> Exp
toExp s op1 op2 | s == ['+'] = Add op1 op2 
                | s == ['-'] = Sub op1 op2
                | s == ['*'] = Prod op1 op2
                | s == ['/'] = Div op1 op2

isNumeric :: Char -> Bool 
isNumeric c = elem c ['0'..'9']

isOperator :: Char -> Bool 
isOperator c = elem c ['+','-','*','/']

-- b) Defina una funcion evalRPN :: String → Int para evaluar expresiones aritmeticas escritas en RPN. 

evalRPN :: String -> Int
evalRPN [] = 0
evalRPN s = eval (parseRPN s)

sevalRPN :: String -> Maybe Int
sevalRPN [] = Just 0
sevalRPN s = seval (parseRPN s)

-- 6) b) Defina un evaluador seval :: Exp → Maybe Int para controlar los errores de division por 0. 

seval :: Exp -> Maybe Int
seval (Lit x) = Just x
seval (Add x y) = case (seval x, seval y) of
        (Just r1, Just r2) -> Just (r1 + r2)
        _                  -> Nothing
seval (Sub x y ) = case (seval x, seval y) of
        (Just r1, Just r2) -> Just (r1 - r2)
        _                  -> Nothing
seval (Prod x y ) = case (seval x, seval y) of
        (Just r1, Just r2) -> Just (r1 * r2)
        _                  -> Nothing
seval (Div x y) = case (seval x, seval y) of
        (Just r1, Just r2) -> if r2 == 0 then Nothing else Just (div r1 r2)
        _                  -> Nothing


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