--TP De Santos Lurati, Maximiliano Zambón y Tomás Demarchi.

import Data.List (sortOn)


data NdTree p = Node (NdTree p) -- subarbol izquierdo
                        p -- punto
                        (NdTree p) -- subarbol derecho
                        Int -- eje 
                        | Empty
                        deriving (Eq, Ord, Show)


class Punto p where
    dimension :: p -> Int                   -- devuelve el numero de coordenadas de un punto
    coord :: Int -> p -> Double             -- devuelve la coordenada k-esima de un punto (comenzando de 0)
------ a:
    dist :: p -> p -> Double                -- calcula la distancia entre dos puntos
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
    coord 1 (P2d (_, b)) = b

instance (Punto Punto3d) where
    dimension p = 3
    coord 0 (P3d (a, _, _)) = a
    coord 1 (P3d (_, b, _)) = b
    coord 2 (P3d (_, _, c)) = c

-- 2:

fromList :: Punto p => [p] -> NdTree p
fromList [] = Empty
fromList (x:xs) = fromListAux (x:xs) 0 (dimension x)
    where 
        fromListAux [] level dim = Empty
        fromListAux [x] level dim = Node Empty x Empty level 
        fromListAux (x:xs) level dim = let (l1, m ,l2) = maxMediana (mediana (sortOn (coord level) (x:xs))) level   -- con sortOn reordenamos la lista en base al eje correspondiente
                                in Node (fromListAux l1 ((level+1) `mod` dim) dim) m (fromListAux l2 ((level+1) `mod` dim) dim) level

        mediana xs = recorrer xs xs [] 
        -- Parto la lista como primera mitad, mediana y segunda mitad, utilizando liebre-tortuga para evitar recorrer 2 veces la lista
        -- Casos base: el puntero rapido llega al final (lista vacia o un elemento) 
        recorrer (m:resto) (_:_:rapido) acc = recorrer resto rapido (m:acc)
        recorrer (m:resto) [_]         acc = (acc, m, resto)      -- longitud impar
        recorrer (m:resto) []          acc = (acc, m, resto)      -- longitud par
        -- Si tengo varios puntos con la misma coordenada que la mediana en el eje,
        -- selecciono como raiz el de mas a la derecha de ellos para mantener el invariante a izquierda (<=)
        maxMediana t@(acc,m,[]) level   = t
        maxMediana t@(acc,m,x:xs) level    | coord level m == coord level x = maxMediana (m:acc,x,xs) level  
                                           | otherwise = t      -- ya se que el de x va a ser mayor que el de m pues estaba ordenada la lista


-- 3: 

-- Consideramos que no se insertan puntos repetidos por lo que discriminamos el caso donde el punto ya se encuentra en el arbol
-- Para poder comparar los puntos sin modificar el tipo de la funcion principal, 
-- recurrimos a una funcion auxiliar que compara los puntos componente a componente
insertar ::Punto p => p -> NdTree p -> NdTree p
insertar p t = insertarAux p t 0
    where
        insertarAux p Empty ejeCorte  = Node Empty p Empty ejeCorte
        insertarAux p t@(Node l p' r eje) ejeCorte | puntosIguales p  p' (dimension p - 1)  = t 
                                                   |coord eje p <= coord eje p'             = Node (insertarAux p l ((eje + 1) `mod` dimension p)) p' r eje
                                                   |otherwise                               = Node l p' (insertarAux p r ((eje + 1) `mod` dimension p)) eje
        puntosIguales p1 p2 0 =  coord 0 p1 == coord 0 p2
        puntosIguales p1 p2 n = (coord n p1 == coord n p2) && puntosIguales p1 p2 (n - 1)
        

--4: 
 
eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar p Empty = Empty
eliminar p t@(Node l p' r level) 
-- Busqueda del punto a eliminar, una vez encontrado llamamos a la funcion auxiliar que se encarga del algoritmo de la eliminacion  
    | p == p'                         = eliminarAux t 
    | coord level p <= coord level p' = Node (eliminar p l) p' r level 
    | otherwise                       = Node l p' (eliminar p r) level 
    where 
        --caso hoja, simplemente la elimino.
        eliminarAux (Node Empty p Empty levelAct)           = Empty 
        -- Busco el candidato, para reemplazar el nodo que quiero eliminar, en el subarbol izquierdo (pues el hijo derecho es Empty)
        eliminarAux (Node l@(Node _ p' _ _) p Empty level)  = let newP = maxIzq l level p' in Node (eliminar newP l) newP Empty level 
        -- Busco el candidato, para reemplazar el nodo que quiero eliminar, en el subarbol derecho 
        eliminarAux (Node l p r@(Node _ p' _ _) level)      = let newP = minDer r level p' in Node l newP (eliminar newP r) level 

        -- Recibe un arbol y un candidato a ser el maximo y devuelve el punto de maxima componente en el eje de la raiz
        maxIzq Empty _ pMax = pMax
        maxIzq (Node l p r levelAct) level pMax | level == levelAct = maxIzq r level (maxP level p pMax) --como estoy en el mismo nivel solo busco a derecha
                                                | otherwise = maxIzq l level (maxP level (maxIzq r level p) pMax) --busco en ambos niveles

        
        -- Recibe un arbol y un candidato a ser el minimo y devuelve el punto de minima componente en el eje de la raiz
        minDer Empty level pMin = pMin
        minDer (Node l p r levelAct) level pMin | level == levelAct = minDer l level (minP level p pMin) --como estoy en el mismo nivel solo busco a izquierda
                                                | otherwise = minDer l level (minP level (minDer r level p) pMin) --busco en ambos niveles
        
        -- Recibe un eje y dos puntos y devuelve el que tiene maxima componente en el eje recibido
        maxP level p1 p2  | coord level p1 > coord level p2 = p1
                          | otherwise = p2

        -- Recibe un eje y dos puntos y devuelve el que tiene minima componente en el eje recibido
        minP level p1 p2  | coord level p1 < coord level p2 = p1
                          | otherwise = p2
        


--5:  

type Rect = (Punto2d, Punto2d) 


-- Consideramos que los puntos del borde no pertenecen a la region y
-- que los puntos del rectangulo no se reciben en un orden especifico
inRegion :: Punto2d -> Rect -> Bool
inRegion (P2d (x,y)) (P2d (x1, y1), P2d (x2, y2)) = 
        let (xMin,xMax) = if x1 <= x2 then (x1,x2) else (x2,x1)
            (yMin,yMax) = if y1 <= y2 then (y1,y2) else (y2,y1)
        in  xMin < x && x < xMax && yMin < y && y < yMax
            

ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty rect = []
ortogonalSearch t rect = ortogonalSearchAux t rect []
      where 
        ortogonalSearchAux Empty rect xs = xs
        ortogonalSearchAux (Node l p r level) rect xs   | coord level p >= maxCoord level rect  = ortogonalSearchAux l rect xs
                                                        | coord level p <= minCoord level rect  = ortogonalSearchAux r rect xs 
                                                        | inRegion p rect                       = ortogonalSearchAux l rect (p : ortogonalSearchAux r rect xs)
                                                        | otherwise                             = ortogonalSearchAux l rect (ortogonalSearchAux r rect xs)
        
        -- maxima y minima coordenada del rectangulo en el eje recibido (extremos del intervalo)
        maxCoord level rect = max (coord level (fst rect)) (coord level (snd rect))
        minCoord level rect = min (coord level (fst rect)) (coord level (snd rect))

