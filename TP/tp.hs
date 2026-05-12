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

        mediana xs = recorrer xs xs [] 
        -- Casos base: el puntero rápido llega al final (lista vacía o un elemento)
        recorrer (m:resto) (_:_:rapido) acc = recorrer resto rapido (m:acc)
        recorrer (m:resto) [_]         acc = (acc, m, resto)      -- longitud impar
        recorrer (m:resto) []          acc = (acc, m, resto)      -- longitud par

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
puntosPrueba = [P2d (2,3), P2d (5,4), P2d (6,4), P2d (7,4) , P2d (9,6), P2d (4,7), P2d (8,1), P2d (7,2)]
            -- (2,3) (4,7) (5,4) (6,4) (7,4) (7,2) (8,1) (9,6)
            --    |         acc            |    m   |      xs      |
arbolList = fromList puntosPrueba
lista2 = [P2d (1,1), P2d (2,0), P2d (2,1), P2d (2,2), P2d (2,10) ]
arbol2 = fromList lista2


-------------------------------------------------------------------------------------------------------------------------------------------

--4:
-- eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
-- eliminar p t = eliminarAux p t 0
--     where eliminarAux p Empty _ = Empty
--           eliminarAux p (Node l p' Empty eje) ejeCorte | coord ejeCorte p < coord ejeCorte p' = Node (eliminarAux p l ((eje + 1) `mod` dimension p)) p' r eje
--                                                        | otherwise 

--           eliminarAux p (Node l p' r eje) ejeCorte | coord ejeCorte p < coord ejeCorte p' = Node (eliminarAux p l ((eje + 1) `mod` dimension p)) p' r eje
--                                                    | coord ejeCorte p > coord ejeCorte p' = Node l p' (eliminarAux p r ((eje + 1) `mod` dimension p)) eje
--                                                    | otherwise 

--5:  

isRegion :: Punto2d -> Rect -> Bool
isRegion (P2d (x,y)) (P2d (x1, y1), P2d (x2, y2)) = 
        let (xMin,xMax) = if x1 <= x2 then (x1,x2) else (x2,x1)
            (yMin,yMax) = if y1 <= y2 then (y1,y2) else (y2,y1)
        in  xMin < x && x < xMax && yMin < y && y < yMax
        
            

ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty rect = []
ortogonalSearch t@(Node l  (P2d (x, y)) r level) rect@(a,b) = ortogonalSearchAux t rect []
      where 
        ortogonalSearchAux Empty rect xs = xs
        ortogonalSearchAux (Node l p r level) rect xs   | pertenece = ortogonalSearchAux l rect (p: (ortogonalSearchAux r rect xs))
                                                        | coord level p > maxCoord level rect   = ortogonalSearchAux l rect xs
                                                        | coord level p < minCoord level rect   = ortogonalSearchAux r rect xs 
                                                        | otherwise                             = ortogonalSearchAux l rect (ortogonalSearch r rect xs)

        pertenece = isRegion p rect
        maxCoord level rect = max (coord level (fst rect)) (coord level (snd rect))
        minCoord level rect = min (coord level (fst rect)) (coord level (snd rect))