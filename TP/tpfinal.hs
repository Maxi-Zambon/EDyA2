--TP De Santos Lurati, Maximiliano Zambón y Tomás Demarchi.

import Data.List (sortOn)


data NdTree p = Node (NdTree p) -- subarbol izquierdo
                        p -- punto
                        (NdTree p) -- subarbol derecho
                        Int -- eje 
                        | Empty
                        deriving (Eq, Ord, Show)


class Punto p where
    dimension :: p -> Int -- devuelve el numero de coordenadas de un punto
    coord :: Int -> p -> Double -- devuelve la coordenada k-esima de un punto (comenzando de 0)
    -- a:
    dist :: p -> p -> Double -- calcula la distancia entre dos puntos
    dist p1 p2 = sqrt(distAux p1 p2 (dimension p1 - 1) 0)
        where 
            distAux p1 p2 0 acc = acc + (coord 0 p1 - coord 0 p2)^ 2
            distAux p1 p2 n acc = distAux p1 p2 (n-1) (acc + (coord n p1 - coord n p2)^ 2)



-- b:

newtype Punto2d = P2d (Double, Double) deriving (Show, Eq)
newtype Punto3d = P3d (Double, Double, Double) deriving (Show, Eq)

instance (Punto Punto2d) where
    dimension p = 2
    coord 0 (P2d (a, _)) = a
    coord 1 (P2d (_, b))= b

instance (Punto Punto3d) where
    dimension p = 3
    coord 0 (P3d (a, _, _)) = a
    coord 1 (P3d (_, b, _))= b
    coord 2 (P3d (_, _, c))= c

-- 2:

fromList :: Punto p => [p] -> NdTree p
fromList [] = Empty
fromList (x:xs) = fromListAux (x:xs) 0 (dimension x)
    where 
        fromListAux [] level dim = Empty
        fromListAux [x] level dim = Node Empty x Empty level 
        fromListAux (x:xs) level dim= let (l1, m ,l2) = maxMediana (mediana (sortOn (coord level) (x:xs))) level   
                                in Node (fromListAux l1 ((level+1) `mod` dim) dim) m (fromListAux l2 ((level+1) `mod` dim) dim) level

        mediana xs = recorrer xs xs [] --liebre y tortuga
        -- Casos base: el puntero rápido llega al final (lista vacía o un elemento)
        recorrer (m:resto) (_:_:rapido) acc = recorrer resto rapido (m:acc)
        recorrer (m:resto) [x] acc = (acc, m, resto)      -- longitud impar
        recorrer (m:resto) []  acc = (acc, m, resto)      -- longitud par

        -- maxMediana ([],m,resto) level  = ([],m,resto)
        maxMediana t@(acc,m,[]) level   = t
        maxMediana t@(acc,m,x:xs) level    | coord level m == coord level x = maxMediana (m:acc,x,xs) level  
                                           | otherwise = t      -- ya se que el de x va a ser mayor que el de m pues estaba ordenada la lista

-- 3: 

insertar ::(Eq p, Punto p) => p -> NdTree p -> NdTree p
insertar p t = insertarAux p t 0
    where
        insertarAux p Empty ejeCorte  = Node Empty p Empty ejeCorte
        insertarAux p t@(Node l p' r eje) ejeCorte | p == p' = t --CONSULTA (¿Se puede cambiar el tipo para evitar insertar repetidos?)
                                                   |coord eje p <= coord eje p' = Node (insertarAux p l ((eje + 1) `mod` dimension p)) p' r eje
                                                   |otherwise = Node l p' (insertarAux p r ((eje + 1) `mod` dimension p)) eje


--------------------------------------------------------------- PARA TESTEO -----------------------------------------------------------

printTree :: Show p => NdTree p -> IO ()
printTree tree = putStr (drawTree tree 0)
  where
    drawTree Empty _ = ""
    drawTree (Node left p right axis) level =
      -- Imprime el subárbol derecho primero (arriba)
      drawTree right (level + 1) ++
      -- Imprime el nodo actual con mucha más sangría y un indicador
      indent level ++ show p ++ " [eje " ++ show axis ++ "]\n" ++
      -- Imprime el subárbol izquierdo (abajo)
      drawTree left (level + 1)
    
    -- Función auxiliar para manejar el espaciado
    indent 0 = "" -- La raíz no tiene sangría
    indent n = replicate (n * 14 - 5) ' ' ++ "|--> "


puntosPrueba :: [Punto2d]
puntosPrueba = [P2d (2,3), P2d (5,4), P2d (6,4), P2d (7,4) , P2d (9,6), P2d (4,7), P2d (8,1), P2d (7,2), P2d (1,1), P2d (2,0), P2d (2,1), P2d (2,2), P2d (2,10)]
            -- (2,3) (4,7) (5,4) (6,4) (7,4) (7,2) (8,1) (9,6)
            --    |         acc            |    m   |      xs      |
arbolList = fromList puntosPrueba

puntosPrueba3D =
    [ P3d (2,3,1)
    , P3d (5,4,2)
    , P3d (6,4,0)
    , P3d (7,4,5)
    , P3d (9,6,3)
    , P3d (4,7,8)
    , P3d (8,1,2)
    , P3d (7,2,9)
    , P3d (1,1,1)
    , P3d (2,0,4)
    , P3d (2,1,7)
    , P3d (2,2,6)
    , P3d (2,10,3)
    , P3d (3,5,8)
    , P3d (10,2,1)
    , P3d (0,0,0)
    , P3d (11,4,7)
    , P3d (6,8,2)
    , P3d (5,9,9)
    , P3d (12,3,4)
    , P3d (4,4,4)
    , P3d (8,8,8)
    , P3d (1,9,2)
    , P3d (3,7,5)
    , P3d (6,0,3)
    , P3d (9,1,6)
    , P3d (13,2,7)
    , P3d (14,5,1)
    , P3d (15,3,9)
    , P3d (16,6,0)
    , P3d (17,7,4)
    , P3d (18,8,2)
    , P3d (19,9,5)
    , P3d (20,1,8)
    , P3d (21,2,6)
    , P3d (22,4,3)
    , P3d (23,5,7)
    , P3d (24,6,1)
    , P3d (25,7,9)
    , P3d (26,8,4)
    ]

arbolList2 = fromList puntosPrueba3D



-------------------------------------------------------------------------------------------------------------------------------------------

--4:
eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar p Empty = Empty
eliminar p t@(Node l p' r level) 
    | p == p'                         = eliminarAux t 
    | coord level p <= coord level p' = Node (eliminar p l) p' r level 
    | otherwise                       = Node l p' (eliminar p r) level 
    where 
        -- eliminarAux Empty level dim = Empty
        eliminarAux (Node Empty p Empty levelAct) = Empty --caso hoja, simplemente la elimino.
        eliminarAux (Node l@(Node _ p' _ _) p Empty level)  = let newP = maxIzq l level p' in Node (eliminar newP l) newP Empty level 
        eliminarAux (Node l p r@(Node _ p' _ _) level)   = let newP = minDer r level p' in Node l newP (eliminar newP r) level 


        maxIzq Empty _ pMax = pMax
        maxIzq (Node l p r levelAct) level pMax | level == levelAct = maxIzq r level (maxP level p pMax) --como estoy en el mismo nivel solo busco a derecha
                                                | otherwise = maxIzq l level (maxP level (maxIzq r level p) pMax) --busco en ambos niveles

        

        minDer Empty level pMin = pMin
        minDer (Node l p r levelAct) level pMin | level == levelAct = minDer l level (minP level p pMin) --como estoy en el mismo nivel solo busco a izquierda
                                                | otherwise = minDer l level (minP level (minDer r level p) pMin) --busco en ambos niveles

        maxP level p1 p2  | coord level p1 > coord level p2 = p1
                          | otherwise = p2

        minP level p1 p2  | coord level p1 < coord level p2 = p1
                          | otherwise = p2
        


--5:  

type Rect = (Punto2d, Punto2d) 


-- Consideramos que los puntos del borde no pertenecen a la region PREGUNTAR EN CLASE (no confian en el tero)
inRegion :: Punto2d -> Rect -> Bool
inRegion (P2d (x,y)) (P2d (x1, y1), P2d (x2, y2)) = 
        let (xMin,xMax) = if x1 <= x2 then (x1,x2) else (x2,x1)
            (yMin,yMax) = if y1 <= y2 then (y1,y2) else (y2,y1)
        in  xMin < x && x < xMax && yMin < y && y < yMax
        -- si no tenemos la seguridad del orden de los puntos del rect, no zafo de hacer esto o si?
            

ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty rect = []
ortogonalSearch t rect = ortogonalSearchAux t rect []
      where 
        ortogonalSearchAux Empty rect xs = xs
        ortogonalSearchAux (Node l p r level) rect xs   | coord level p >= maxCoord level rect  = ortogonalSearchAux l rect xs
                                                        | coord level p <= minCoord level rect  = ortogonalSearchAux r rect xs 
                                                        | inRegion p rect                       = ortogonalSearchAux l rect (p : ortogonalSearchAux r rect xs)
                                                        | otherwise                             = ortogonalSearchAux l rect (ortogonalSearchAux r rect xs)

        maxCoord level rect = max (coord level (fst rect)) (coord level (snd rect))
        minCoord level rect = min (coord level (fst rect)) (coord level (snd rect))
        -- Esto se podria evitar calcular en cada llamada, pensar como


-- ==========================================
-- SCRIPT DE TESTING ESTRICTO (Fail-Fast)
-- ==========================================

-- Función auxiliar que corta el programa si algo no coincide
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual nombreTest obtenido esperado = do
    putStr $ "Test: " ++ nombreTest ++ "... "
    if obtenido == esperado
        then putStrLn "OK ✔️"
        else error $ "\n\n❌ ERROR FATAL ❌\nFalló en el test: " ++ nombreTest ++ 
                     "\n  Se esperaba: " ++ show esperado ++ 
                     "\n  Se obtuvo:   " ++ show obtenido ++ "\n"

main :: IO ()
main = do
    putStrLn "========================================"
    putStrLn "  INICIANDO BATERÍA DE TESTS ESTRICTOS"
    putStrLn "========================================\n"

    let puntos = [P2d (2,3), P2d (5,4), P2d (9,6), P2d (4,7), P2d (8,1), P2d (7,2)]
    let arbol = fromList puntos

    -- TEST 1: Distancia (triángulo pitagórico clásico 3-4-5)
    assertEqual "Distancia euclídea de (0,0) a (3,4)" (dist (P2d (0,0)) (P2d (3,4))) 5.0

    -- TEST 2: Inserción de duplicados
    -- Como definieron que si p == p' devuelve t, insertar la raíz de nuevo no debería cambiar el árbol
    assertEqual "Insertar un punto duplicado devuelve el mismo árbol" (insertar (P2d (7,2)) arbol) arbol

    -- TEST 3: Búsqueda Ortogonal
    let rectBusqueda = (P2d (1,1), P2d (6,6))
    let encontrados = ortogonalSearch arbol rectBusqueda
    -- Ordenamos ambas listas con 'sort' para que el assert no falle si el árbol recolectó los puntos en otro orden
    let esperadosTest3 = sortOn (coord 0) [P2d (2,3), P2d (5,4)]
    assertEqual "Búsqueda ortogonal en región (1,1)-(6,6)" (sortOn (coord 0) encontrados) esperadosTest3

    -- TEST 4: Búsqueda de un punto fuera de rango
    let rectVacio = (P2d (10,10), P2d (20,20))
    assertEqual "Búsqueda ortogonal en región sin puntos" (ortogonalSearch arbol rectVacio) []

    -- TEST 5: Eliminación de una hoja
    let arbolSinHoja = eliminar (P2d (4,7)) arbol
    -- Si lo eliminé bien, hacer una búsqueda ortogonal justo en ese punto debería devolver vacío
    let busquedaPostEliminar = ortogonalSearch arbolSinHoja (P2d (3,6), P2d (5,8))
    assertEqual "Eliminar hoja (4,7) hace que no se encuentre más" busquedaPostEliminar []

    -- TEST 6: Eliminación de la raíz preserva a los hijos
    let arbolSinRaiz = eliminar (P2d (7,2)) arbol
    -- Si elimino la raíz (7,2), el punto (9,6) que estaba a la derecha no debe desaparecer del árbol
    -- Buscamos en una región que envuelva exclusivamente al (9,6)
    let busquedaHijoSobreviviente = ortogonalSearch arbolSinRaiz (P2d (8,5), P2d (10,7))
    assertEqual "Eliminar raíz (7,2) preserva la existencia del hijo (9,6)" busquedaHijoSobreviviente [P2d (9,6)]

    putStrLn "\n========================================"
    putStrLn " 🎉 TODOS LOS TESTS PASARON CON ÉXITO 🎉"
    putStrLn "========================================"