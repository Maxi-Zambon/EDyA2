
data CList a = EmptyCL | CUnit a | Concat a (CList a) a deriving Show

headCL :: CList a -> a
headCL (CUnit a) = a
headCL (Concat x l y) = x

isEmpty :: CList a -> Bool

isEmpty (CUnit a) = False
isEmpty (Concat x l y) = False
isEmpty _ = True


concatCL :: CList (CList a) -> CList a
concatCL EmptyCL = EmptyCL