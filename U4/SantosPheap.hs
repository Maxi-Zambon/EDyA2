
data PHeaps a = Empty | Root a [PHeaps a ]

isPHeap :: Ord a => PHeaps a -> Bool
isPHeap Empty = True
 -- (abajo) la idea del foldr es llevar un acumulador. No operar entre 2 elementos.
isPHeap h@(Root a xs)  =  checkRoot h && foldr (\x acc -> isPHeap x && acc) True xs  
            where   
                checkRoot (Root a xy) = foldr (\x acc -> raiz x >= a && acc) True xy
                    where 
                        raiz (Root v _) = v
                        raiz Empty      = a -- Si es Empty, devolvemos 'a' para que (a <= a) sea True y no rompa

--a priori anda, pero usando all que verifica un predicado en toda la lista:
isPHeapPro :: Ord a => PHeaps a -> Bool
isPHeapPro (Root a xs) = 
      all (\hijo -> a <= raiz hijo) xs   -- ¿todos los hijos directos cumplen?
   && all isPHeap xs                     -- ¿todos los sub‑heaps son válidos?
  where
    raiz (Root v _) = v
    raiz Empty      = a                  -- un hijo vacío no rompe nada


isPHeapPro2 :: Ord a => PHeaps a -> Bool
isPHeapPro2 Empty = True
isPHeapPro2 (Root x hijos) =
    all esHeapValido hijos && all isPHeap hijos
  where
    esHeapValido Empty          = True
    esHeapValido (Root y _)     = x <= y   -- asumiendo min-heap

merge :: Ord a => PHeaps a -> PHeaps a -> PHeaps a
merge a Empty = a
merge Empty a = a
merge h1@(Root a xs) h2@(Root b ys) = if a <= b then Root a (h2:xs)
                                    else Root b (h1:ys)

insert :: Ord a => PHeaps a -> a -> PHeaps a
insert h a = merge (Root a []) h

concatHeaps :: Ord a => [PHeaps a ] -> PHeaps a
--puesto que el costo del merge es 1, no hay tanta diferencia si mergeo de a pares o no
concatHeaps [] = Empty
concatHeaps (x:xs) = foldr merge Empty xs


--version pro para que el heap quede mas "balanceado" (no lo pide el ejercicio)
--de esta forma el delete min queda amortizado a lgn, mientras que la anterior es de n.
--pero parece ser común esta idea de mergear de a pares.
-- Pasada 1: Mergear de a pares
-- Pasada 2: Mergear los resultados
mergePairs :: Ord a => [PHeaps a] -> PHeaps a
mergePairs []           = Empty
mergePairs [h]          = h
mergePairs (h1:h2:rest) = merge (merge h1 h2) (mergePairs rest)



--a raiz de que me estaba volviendo loco para hacerlo, recurro a que la ia me de una mano.
--puesto que en este caso (diferente es en un parcial) no me conformo con una solucion desprolija.
delMin :: Ord a => PHeaps a -> Maybe (a, PHeaps a)
delMin Empty = Nothing
delMin (Root x hijos) = Just (x, mergePairs hijos) 
--si yo "mergeo los hijos" me queda un heap y esta balanceado.
--realmente era la forma mas facil y el costo es lgn puesto que merge es o(1)
--pero bueno, la verdad que no la vi.





