module T where

-- Definition 3
type Trace = [String]
type Delta  = [Trans]
data Trans = Insert String | Skip | Modify String | Delete
trans :: (Trace, Trace) -> Delta
trans ([],[]) = []
trans ([],xs) = map Insert xs
trans (xs,[]) = map (const Delete) xs
trans ((x:xs),(y:ys)) 
   | x == y = Skip:trans (xs,ys)
   | otherwise = Modify y:trans (xs,ys)

-- Definition 4
type Label = String
type ATrace = [(String,Label)]
type Order = Label -> Label -> Bool
compose :: Order -> ATrace -> ATrace -> Maybe [ATrace]
compose po t1 t2 = compose' po t1 t2 (Just [])

compose' :: Order -> ATrace -> ATrace -> Maybe [ATrace] -> Maybe [ATrace]
compose' po [] t2 ts = fmap (map (++t2)) ts 
compose' po t1 [] ts = fmap (map (++t1)) ts 
compose' po t1@((v1,l1):t1') t2@((v2,l2):t2') ts 
  | l1 == l2 && v1 == v2 = compose' po t1' t2' (fmap (combine [(v1,l1)]) ts)
  | l1 == l2 && v1 /= v2 = Nothing
  | po l1 l2 = compose' po t1' t2 (fmap (combine [(v1,l1)]) ts)
  | po l2 l1 = compose' po t1 t2' (fmap (combine [(v2,l2)]) ts)
  | v1 == v2 = compose' po t1' t2' 
          (fmap (\ts' -> combine [(v1,l1)] ts' ++ combine [(v2,l2)] ts') ts)
  | otherwise = Nothing 

combine :: ATrace -> [ATrace] -> [ATrace]
combine t [] = [t]
combine t ts = map (++t) ts

tA,tB :: ATrace
tA = [("hello world","1"),("adios","1i")]
tB = [("hello world","1"),("bye world","2i")]
ex :: Maybe [ATrace]
ex = compose (<) tA tB 
