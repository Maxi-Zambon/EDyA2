
module Lab2 where

{-
   Laboratorio 2
   EDyAII 2022
-}

import Data.List

-- 1) Dada la siguiente definición para representar árboles binarios:

data BTree a = E | Leaf a | Node (BTree a) (BTree a)

-- Definir las siguientes funciones:

-- a) altura, devuelve la altura de un árbol binario.

altura :: BTree a -> Int
altura E = -1
altura (Leaf a) = 0
altura (Node l r) = 1 + max (altura l) (altura r)

-- b) perfecto, determina si un árbol binario es perfecto (un árbol binario es 
-- perfecto si cada nodo tiene 0 o 2 hijos y todas las hojas están a la misma 
-- distancia desde la raı́z).

perfecto :: BTree a -> Bool
perfecto E = True
perfecto (Leaf _) = True
perfecto (Node l r) = altura l == altura r && perfecto l && perfecto r

-- c) inorder, dado un árbol binario, construye una lista con el recorrido 
-- inorder del mismo.

inorder :: BTree a -> [a]
inorder E = []
inorder (Leaf x) = [x]
inorder (Node l r) = inorder l ++ inorder r 

-- 2) Dada las siguientes representaciones de árboles generales y de árboles 
-- binarios (con información en los nodos):

data GTree a = EG | NodeG a [GTree a] deriving Show

data BinTree a = EB | NodeB (BinTree a) a (BinTree a) deriving Show

{- Definir una función g2bt que dado un árbol nos devuelva un árbol binario de 
   la siguiente manera:
   la función g2bt reemplaza cada nodo n del árbol general (NodeG) por un nodo 
   n' del árbol binario (NodeB ), donde el hijo izquierdo de n' representa el 
   hijo más izquierdo de n, y el hijo derecho de n' representa al hermano 
   derecho de n, si existiese (observar que de esta forma, el hijo derecho de 
   la raı́z es siempre vacı́o).
   
   Por ejemplo, sea t: 
       
                    A 
                 / | | \
                B  C D  E
               /|\     / \
              F G H   I   J
             /\       |
            K  L      M    
   
   g2bt t =
         
                  A
                 / 
                B 
               / \
              F   C 
             / \   \
            K   G   D
             \   \   \
              L   H   E
                     /
                    I
                   / \
                  M   J  
-}

g2bt :: GTree a -> BinTree a
g2bt EG = EB
g2bt (NodeG x []) = NodeB EB x EB -- Resolvemos el caso de la raiz
g2bt (NodeG x sons) = NodeB (g2btAux (head sons) (tail sons)) x EB 

-- Toma la lista de hermanos
g2btAux :: GTree a -> [GTree a] -> BinTree a
g2btAux EG [] = EB
-- Si tengo un nodo EG en la lista que tiene hermanos simplemente 
-- ignoro este nodo.
g2btAux EG rigthBrothers = g2btAux (head rigthBrothers) (tail rigthBrothers)
g2btAux (NodeG x []) [] = NodeB EB x EB -- Nodo sin hijos y sin hermanos
-- Nodo sin hijos y con hermanos
g2btAux (NodeG x []) rigthBrothers = 
   NodeB EB x (g2btAux (head rigthBrothers) (tail rigthBrothers))
-- Nodo con hijos y sin hermanos
g2btAux (NodeG x sons) [] = NodeB (g2btAux (head sons) (tail sons)) x EB

g2btAux (NodeG x nodes) rigthBrothers = 
   NodeB (g2btAux (head nodes) (tail nodes)) 
         x 
         (g2btAux (head rigthBrothers) (tail rigthBrothers))

arbolEjemplo :: GTree Char
arbolEjemplo = 
  NodeG 'A' [
    NodeG 'B' [
      NodeG 'F' [NodeG 'K' [], NodeG 'L' []],
      NodeG 'G' [],
      NodeG 'H' []
    ],
    NodeG 'C' [],
    NodeG 'D' [],
    NodeG 'E' [
      NodeG 'I' [NodeG 'M' []],
      NodeG 'J' []
    ]
  ]

-- Solucion IA-----------------------------------------------------------------

-- g2bt :: GTree a -> BinTree a
-- g2bt EG = EB
-- -- La raíz nunca tiene hermanos, por eso el hijo derecho es EB.
-- g2bt (NodeG x sons) = NodeB (g2btAux sons) x EB

-- -- Esta función auxiliar ahora toma la LISTA completa de hermanos.
-- g2btAux :: [GTree a] -> BinTree a
-- g2btAux [] = EB
-- -- Si el primer elemento es un árbol vacío, lo ignoramos y seguimos.
-- g2btAux (EG : brothers) = g2btAux brothers
-- -- El primer hijo de la lista se convierte en un NodeB.
-- -- Su izquierdo son SUS propios hijos.
-- -- Su derecho son los HERMANOS que le seguían en la lista.
-- g2btAux (NodeG x sons : brothers) = 
--   NodeB (g2btAux sons) x (g2btAux brothers)

-- 3) Utilizando el tipo de árboles binarios definido en el ejercicio anterior, 
-- definir las siguientes funciones: 
{-
   a) dcn, que dado un árbol devuelva la lista de los elementos que se 
   encuentran en el nivel más profundo que contenga la máxima cantidad de 
   elementos posibles. Por ejemplo, sea t:
            1
          /   \
         2     3
          \   / \
           4 5   6

   dcn t = [2, 3], ya que en el primer nivel hay un elemento, en el segundo 2 
   siendo este número la máxima cantidad de elementos posibles para este nivel
   y en el nivel tercer hay 3 elementos siendo la cantidad máxima 4.
-}

data BinTree a = EB | NodeB (BinTree a) a (BinTree a) deriving Show

dcn :: BinTree a -> [a]
dcn = undefined

{- b) maxn, que dado un árbol devuelva la profundidad del nivel completo
      más profundo. Por ejemplo, maxn t = 2 
-}

maxn :: BinTree a -> Int
maxn = undefined

{- c) podar, que elimine todas las ramas necesarias para transformar
      el árbol en un árbol completo con la máxima altura posible. 
      Por ejemplo,
         podar t = NodeB (NodeB EB 2 EB) 1 (NodeB EB 3 EB)
-}

podar :: BinTree a -> BinTree a
podar = undefined
