{-
1. Si un arbol binario es dado como un nodo con dos subarboles identicos se puede aplicar la tecnica sharing para
que los subarboles sean representados por el mismo arbol. Definir las siguientes funciones de manera que se puedan
compartir la mayor cantidad posible de elementos de los arboles creados:
-}

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

-- a) completo :: a → Int → Tree a, tal que dado un valor x de tipo a y un entero d, crea un arbol binario completo
--  de altura d con el valor x en cada nodo.

completo :: a -> Int -> Tree a
completo x 0 = Leaf
completo x d = Node t x t where t = completo x (d - 1)

-- b) balanceado::a → Int → Tree a, tal que dado un valor x de tipo a y un entero n, crea un arbol binario balanceado
--  de tamaño n, con el valor x en cada nodo.

balanceado :: a -> Int -> Tree a
balanceado x 0 = Leaf
balanceado x n  | odd n  = Node t x t 
                | otherwise = Node t1 x t2 
                    where   t = balanceado x (div n 2)
                            t1 = balanceado x (div n 2) 
                            t2 = balanceado x (div n 2 - 1)
                                                

-- 2. Definir las siguientes funciones sobre arboles binarios de busqueda (bst):

type BST = Tree

-- 1. maximum :: Ord a ⇒ BST a → a, que calcula el maximo valor en un bst.


mini :: Ord a => BST a -> a 
mini (Node Leaf x rt) = x
mini (Node lt x rt) = mini lt

maxi :: Ord a => BST a -> a 
maxi (Node lt x Leaf) = x
maxi (Node lt x rt) = maxi rt

-- 2. checkBST :: Ord a ⇒ BST a → Bool, que chequea si un arbol binario es un bst
-- Salvo todos los casos donde tengo hojas pues mis minimum y maximum no estan definidas para hojas
checkBST :: Ord a => BST a -> Bool
checkBST Leaf = True
checkBST (Node Leaf x Leaf) = True
checkBST (Node Leaf x r) = (x < maxi r) && checkBST r
checkBST (Node l x Leaf) = (mini l <= x) && checkBST l
checkBST (Node l x r) = (mini l <= x) && checkBST l && (x < maxi r) && checkBST r 

-- 3. splitBST :: Ord a ⇒ BST a → a → (BST a, BST a), que dado un arbol bst t y un elemento x , devuelva una
-- tupla con un bst con los elementos de t menores o iguales a x y un bst con los elementos de t mayores a x .
splitBST :: Ord a => BST a -> a -> (BST a, BST a)
splitBST Leaf _ = (Leaf,Leaf)
splitBST i@(Node l a r) x 
    | a == x = (Node l a Leaf, r)
    | x > a = let (l2,r2) = splitBST r x in (Node l a l2, r2)
    | x < a = let (l2,r2) = splitBST l x in (l2, Node r2 a r)

-- 4. join :: Ord a ⇒ BST a → BST a → BST a, que una los elementos dos arboles bst en uno.
join :: Ord a => BST a -> BST a -> BST a
join Leaf Leaf = Leaf
join Leaf x = x 
join x Leaf = x
join t1 (Node l x r) = let (lx,rx) = splitBST t1 x in Node (join l lx) x (join r rx)

-- -- 3. La definicion de member dada en teorıa (la cual determina si un elemento esta en un bst) realiza en el peor
-- caso 2 ∗ d comparaciones, donde d es la altura del arbol. Dar una definicion de member que realice a lo sumo d + 1
-- comparaciones. Para ello definir member en terminos de una funcion auxiliar que tenga como parametro el elemento
-- candidato, el cual puede ser igual al elemento que se desea buscar (por ejemplo, el ultimo elemento para el cual la
-- comparacion de a <= b retorno True) y que chequee que los elementos son iguales solo cuando llega a una hoja del
-- arbol.

memberMala :: Ord a => BST a -> a -> Bool
memberMala Leaf _ = False
memberMala (Node l x r) y   | x == y = True
                            | x < y = memberMala l y
                            | otherwise = memberMala r y
--Member que cumple con la consigna
member :: Ord a => a -> BST a -> Bool
member x  Leaf = False
member x t@(Node l a r) = member2 x t a

member2 :: Ord a => a -> BST a -> a -> Bool 
member2 x Leaf c = x == c
member2 x (Node l y r) c | x < y = member2 x l c
                         | otherwise = member2 x r y
{-
4. La funcion insert dada en teorıa para insertar un elemento en un rbt puede optimizarse eliminando comparaciones
innecesarias hechas por la funcion balance. Por ejemplo, en la definicion de la funcion ins cuando se aplica balance
sobre el resultado de aplicar insert x sobre el subarbol izquierdo (l) y el subarbol derecho (r ), los casos de balance
para testear que se viola el invariante 1 en el subarbol derecho no son necesarios dado que r es un rbt.
-}

data Color = R|B
data RBT a= E | T Color (RBT a) a (RBT a)

-- a) Definir dos funciones lbalance y rbalance que chequeen que el invariante 1 se cumple en los subarboles izquierdo
-- y derecho respectivamente.

lbalance :: Color -> RBT a -> a -> RBT a -> RBT a
-- Caso 1: Violación en el nieto izquierdo-izquierdo
lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
-- Caso 2: Violación en el nieto izquierdo-derecho
lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
-- Si no hay violación o el color es Rojo, no hacemos nada
lbalance c l x r = T c l x r

rbalance :: Color -> RBT a -> a -> RBT a -> RBT a
-- Caso 3: Violación en el nieto derecho-izquierdo
rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
-- Caso 4: Violación en el nieto derecho-derecho
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
-- Caso general
rbalance c l x r = T c l x r

insert :: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t)
        where   ins x E = T R E x E
                ins x (T c l y r ) 
                        | x < y = lbalance c (ins x l) y r
                        | x > y = rbalance c l y (ins x r )
                        | otherwise = T c l y r
                makeBlack E = E
                makeBlack (T _ l x r) = T B l x r

{-
5. Los arboles 1-2-3 son arboles binarios de busqueda donde los nodos pueden guardar multiples valores y tener
entre 2 y 4 hijos.
Especıficamente, en un arbol 1-2-3 los nodos internos son de la forma:
2-node : Contienen un valor y dos hijos.
3-node : Contienen dos valores y tres hijos.
4-node : Contienen tres valores y cuatro hijos
-}
-- 1. Definir un tipo de datos que represente arboles 1-2-3
data Tree123 a = Empty
               | Node2 (Tree123 a) a (Tree123 a)
               | Node3 (Tree123 a) a (Tree123 a) a (Tree123 a)
               | Node4 (Tree123 a) a (Tree123 a) a (Tree123 a) a (Tree123 a)
               deriving (Show, Eq) --son de busqueda.

-- 2. Definir una funcion que transforme red-black trees en arboles 1-2-3. Paralelizar cuando sea posible.

rbto123 :: RBT a -> Tree123 a
rbto123 E = Empty
rbto123 (T B (T R a x b) y (T R c z d)) = Node4 (rbto123 a) x (rbto123 b) y (rbto123 c) z (rbto123 d)
rbtTo123 (T B (T R a x b) y d) = Node3 (rbtTo123 a) x (rbtTo123 b) y (rbtTo123 d)
rbtTo123 (T B a x (T R b y c)) = Node3 (rbtTo123 a) x (rbtTo123 b) y (rbtTo123 c)
rbtTo123 (T B a x b) = Node2 (rbtTo123 a) x (rbtTo123 b)

{-
6. Definir una funcion fromList :: [a ] → Heap a, que cree un leftist heap a partir de una lista, convirtiendo cada
elemento de la lista en un heap de un solo elemento y aplicando la funcion merge hasta obtener un solo heap. Aplicar
la funcion merge n veces, donde n es la longitud de la lista que recibe como argumento la funcion.
-}

type Rank = Int
data Heap a = EmptyH | N Rank a (Heap a) (Heap a)


rank :: Heap a -> Rank
rank EmptyH = 0
rank (N r _ _ _) = r

makeH :: a -> Heap a -> Heap a -> Heap a
makeH x a b = if rank a >= rank b then N (rank b + 1) x a b
                                else N (rank a + 1) x b a

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 EmptyH =  h1
merge EmptyH h2 = h2
merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) =
        if x <= y then makeH x a1 (merge b1 h2) --mmakeH pone a la izquieda el mas largo
        else makeH y a2 (merge h1 b2)


fromList :: Ord a => [a] -> Heap a
fromList [] = EmptyH
fromList (x:xs) = merge (N 1 x EmptyH EmptyH) (fromList xs)

{-
7. Un pairing heap es un ´arbol general que satisface el invariante de heap.
Para implementar pairing heap en Haskell definimos el siguiente tipo de datos:
-}

data PHeaps a = V| Root a [PHeaps a ]

-- 1. isPHeap :: Ord a ⇒ PHeaps a → Bool, determina si un ´arbol es un pairing heap, es decir cumple con el
-- invariante de heap.

isPHeap :: Ord a => PHeaps a -> Bool
isPHeap V = True
isPHeap (Root x lh) =  all isChildValid lh && all isPHeap lh
        where   isChildValid  V = True 
                isChildValid (Root y lhs) = x <= y 

-- 2. merge :: Ord a ⇒ PHeaps a → PHeaps a → PHeaps a, que una dos pairing heap. Para ello, comparar las ra´ıces
-- de ambos ´arboles y elegir la menor como ra´ız del nuevo heap, agregar el ´arbol con mayor ra´ız como hijo de
-- ´este.

phmerge :: Ord a => PHeaps a -> PHeaps a -> PHeaps a
phmerge V y = y
phmerge x V = x
phmerge h1@(Root x hs1) h2@(Root y hs2) | x <= y = Root x (h2:hs1)
                                        | otherwise = Root y (h1:hs2)

-- 3. insert :: Ord a ⇒ PHeaps a → a → PHeaps, que inserte un elemento en un pairing heap. Puede ser ´util la
-- funci´on merge.

phinsert :: Ord a => PHeaps a -> a -> PHeaps a
phinsert hs y = phmerge hs (Root y [])

-- 4. concatHeaps :: Ord a ⇒ [PHeaps a ] → PHeaps a, que dada una lista de pairing heaps construya otro con los
-- elementos del mismo.

concatHeaps :: Ord a => [PHeaps a] -> PHeaps a
concatHeaps [] = V
concatHeaps (h:hs) = phmerge h (concatHeaps hs)

--version pro para que el heap quede mas "balanceado" (no lo pide el ejercicio)
--de esta forma el delete min queda amortizado a lgn, mientras que la anterior es de n.
--pero parece ser común esta idea de mergear de a pares.
-- Pasada 1: Mergear de a pares
-- Pasada 2: Mergear los resultados
mergePairs :: Ord a => [PHeaps a] -> PHeaps a
mergePairs []           = V
mergePairs [h]          = h
mergePairs (h1:h2:rest) = phmerge (phmerge h1 h2) (mergePairs rest)

-- 5. delMin :: Ord a ⇒ PHeaps a → Maybe (a, PHeaps a), que dado un pairing heap, devuelva si el ´arbol no es
-- vac´ıo un par con el menor elemento y un pairing heap sin ´este elemento, o Nothing en otro caso
delMin :: Ord a => PHeaps a -> Maybe (a, PHeaps a)
delMin V = Nothing
delMin (Root x hs) = Just (x, mergePairs hs)