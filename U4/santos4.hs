








data Tree a = Leaf 
            | Node (Tree a) a (Tree a)
            deriving (Show)

type BST = Tree


completo :: a -> Int -> Tree a
completo x 0 = Leaf
completo x d = Node (completo x (d-1)) x (completo x (d-1))

-- completo_pro :: a → Int → Tree a
-- completo_pro x 0 = Leaf
-- completo_pro x d = Node t@(completo x (d-1)) a t
-- el @ esta mal usado pq es para pattern matching. "etiqueta un patron"

--la solucion correcta esta abajo con el where. Esta sí hace que t "referencie". a la llamada recursiva.
completo_pro2 :: a -> Int -> Tree a
completo_pro2 x 0 = Leaf
completo_pro2 x d = Node t x t 
                    where t = completo x (d-1)

balanceado::a -> Int -> Tree a
--fijate que aca la el entero representa la cantidad de nodos no la altura.
balanceado x 0 = Leaf
balanceado x 1 = Node Leaf x Leaf
balanceado x 2 = Node Leaf x (Node Leaf x Leaf)
balanceado x n = if even n then Node (balanceado x (div n 2)) x (balanceado x (div n 2 - 1))
                                else Node t x t where t = balanceado x (div n 2)


--2: 
maxi :: Ord a => BST a -> a
maxi (Node _ x Leaf) = x
maxi (Node l x r) = maxi r

mini :: Ord a => BST a -> a
mini (Node Leaf x _) = x
mini (Node l x r) = mini l

checkBST :: Ord a => BST a -> Bool
checkBST Leaf = True
checkBST (Node Leaf x Leaf) = True
checkBST (Node Leaf x r) = x < maxi r && checkBST r 
checkBST (Node l x Leaf) = x >= mini l && checkBST l
checkBST (Node l x r) = x < maxi r && checkBST r && x >= mini l && checkBST l


--splitBST :: Ord a ⇒ BST a → a → (BST a, BST a), que dado un ´arbol bst t y un elemento x , devuelva una
--tupla con un bst con los elementos de t menores o iguales a x y un bst con los elementos de t mayores a x .
splitBST :: Ord a => BST a -> a -> (BST a, BST a)
splitBST Leaf x = (Leaf, Leaf)
splitBST (Node l y r) x 
        | x == y = (l, r) --evito los duplicados para el join.
        | x < y = let (l1, r1) = splitBST l x in (l1, Node r1 y r)
        | x > y = let (l1, r1) = splitBST r x in  (Node l y l1, r)
            
--no me iba a salir en la vida. Es bastante díficil de entender.


--join :: Ord a ⇒ BST a → BST a → BST a, que una los elementos dos arboles bst en uno

join :: Ord a => BST a -> BST a -> BST a
join Leaf Leaf = Leaf
join x Leaf = x
join Leaf y = y
join t1 (Node l2 y r2) =  let (lx, rx) = splitBST t1 y  in Node (join lx l2) y (join rx r2)

--como me cuesta esto.
--la otra forma (la que habría hecho yo en una situacion desesperada, es pasar el arbol a lista in order, mergear las listas
-- y volver a pasar a árbol)

--3: Me lo debería acordar. 
member1 :: Ord a => BST a -> a -> Bool
member1 Leaf a = False
member1 (Node l x r) a
        | x == a = True
        | x < a = member1 l a
        | x > a = member1 r a

member2_aux :: Ord a  => BST a -> a ->a -> a
member2_aux Leaf a c = c
member2_aux (Node l x r) a c
            |a < x = member2_aux l a c
            |otherwise = member2_aux r a x --actualizo el candidato


member2 :: Ord a => BST a -> a -> Bool
member2 Leaf a = False
member2 t@(Node l x r) a = c == a  where c = member2_aux t a x 

--pectacular.

--4:
data Color = R | B
data RBT a = E | T Color (RBT a) a (RBT a)

balanceL :: Color -> RBT a -> a -> RBT a -> RBT a
balanceL B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balanceL B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balanceL c l a r = T c l a r

balanceR :: Color -> RBT a -> a -> RBT a -> RBT a
balanceR B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balanceR B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balanceR c l a r = T c l a r


insert :: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t)
        where   ins x E = T R E x E
                ins x (T c l y r ) 
                        | x < y = balanceL c (ins x l) y r
                        | x > y = balanceR c l y (ins x r )
                        | otherwise = T c l y r
                makeBlack E = E
                makeBlack (T _ l x r) = T B l x r


--5:
data Tree123 a = Empty
               | Node2 (Tree123 a) a (Tree123 a)
               | Node3 (Tree123 a) a (Tree123 a) a (Tree123 a)
               | Node4 (Tree123 a) a (Tree123 a) a (Tree123 a) a (Tree123 a)
               deriving (Show, Eq) --son de busqueda.


--el rbt ya esta ordenado. No tengo que reacomodar nada, sino valerme de eso.
--la raiz no deberia ser roja.
-- rbt_to_123 :: RBT a -> Tree123 a
-- rbt_to_123 E = Empty
-- rbt_to_123 (T B (T R l1 b r1) a (T R l2 c r2)) = Node4 (rbt_to_123 l1) b (rbt_to_123 r1) a (rbt_to_123 l2) c (rbt_to_123 r2)
-- rbt_to_123 (T B (T R l1 b r1) a r) = Node3 (rbt_to_123 l1) b (rbt_to_123 r1) a (rbt_to_123 r)
-- rbt_to_123 (T B l a (T R l1 b r1)) = Node3 (rbt_to_123 l) a (rbt_to_123 l1) b (rbt_to_123 r1)
-- rbt_to_123 (T B l a r) = Node2 (rbt_to_123 l) a (rbt_to_123 r) 


--peeeero fijate que aca no puedo paralelizar porque: Node2 (rbt_to_123 l) ||a || (rbt_to_123 r)
--tengo el a en el medio que me rompe los huevos. Vamos a intentar solucionarlo.

--gemini me lo tiro tipo:
-- rbt_to_123 (T B (T R l1 b r1) a (T R l2 c r2)) =
--     let t1 = rbt_to_123 l1
--         t2 = rbt_to_123 r1
--         t3 = rbt_to_123 l2
--         t4 = rbt_to_123 r2
--     -- Disparamos 3 chispas y el hilo actual se encarga de la cuarta (t4)
--     in t1 `par` t2 `par` t3 `pseq` (Node4 t1 b t2 a t3 c t4)
--no se que sintaxis espera la catedra.


--6:
type Rank = Int
data Heap a = V | N Rank a (Heap a) (Heap a)

rank :: Heap a -> Rank
rank V = 0
rank (N r _ _ _) = r

makeH :: a -> Heap a -> Heap a -> Heap a
makeH x a b = if rank a >= rank b then N (rank b + 1) x a b
                else N (rank a + 1) x b a

--no puedo ser tan estupido. make pone la rama mas larga a la izquierda.

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1  V= h1
merge V h2 = h2
merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) =
        if x <= y then makeH x a1 (merge b1 h2) --mmakeH pone a la izquieda el mas largo
        else makeH y a2 (merge h1 b2)

--entoneces bien, digamos que esto lo entiendo.

fromList :: Ord a => [a] -> Heap a
-- fromList []     = V
-- fromList (x:xs) = merge (makeH x V V) (fromList xs)
--esta forma no esta mal, pero hay una mejor. (esta es n lgn)

-- fromList [] = V
-- fromList xs = mergeArr (map (\x -> makeH x V V) xs)
--         where mergeArr [] = V
--               mergeArr (x:xs) = merge x (mergeArr xs)

--dice el gemini que sigue siendo n lgn :(
-- y tiene razon, porque estoy recorriendo un array de heaps (n)
--y mergeandolos (lgn)
--vamos a intentar pensarla en n.


fromList [] = V
fromList xs = mergeArr (map (\x -> makeH x V V) xs)
  where 
    mergeArr []  = V
    mergeArr [h] = h
    -- Aquí está el truco: hacemos una pasada de pares y REPETIMOS mergeArr
    mergeArr hs  = mergeArr (pares hs)

    pares (h1:h2:rest) = merge h1 h2 : pares rest
    pares hs           = hs -- Por si queda uno solo al final de la lista

--la verdad que bastante dificil.

showTree :: Show a => Int -> Tree a -> String
showTree n Leaf = replicate n ' ' ++ "|- Leaf"
showTree n (Node l x r) = replicate n ' ' ++ "|- " ++ show x ++ "\n" 
                          ++ showTree (n+2) l ++ "\n" 
                          ++ showTree (n+2) r

-- Para usarlo: putStrLn (showTree 0 miArbol)


