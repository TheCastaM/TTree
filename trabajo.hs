data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v)
                | Leaf k v
                | E deriving Show

{-
    La estructura que se usa para crear el diccionario es un arbol
    de tres hojas donde cada Node o hoja tiene un caracter de el 
    codigo guardado en "k" y un posible valor guardado en "v" que 
    existe en el caso de que el codigo este asociado a ese valor.
-}

{-
t = Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E)(Node 'o' (Just 2) (Leaf 'd' 9)E(Leaf 's' 4))E)(Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8)(Leaf 'n' 7)E)E)
-}


{-
    serch devuelve el valor asociado a una clave.
-}
serch :: Ord k => [k] -> TTree k v -> Maybe v
serch _ E = Nothing
serch (c:cs) (Leaf k val) = if ((c == k) && (cs == [])) then Just val else Nothing
serch (c:cs) (Node k val l m r) | (c == k) = if (cs == []) then val else serch cs m
                                | (c < k) = serch (c:cs) l
                                | (c > k) = serch (c:cs) r
                                | otherwise = Nothing


isEmpty :: Ord k => TTree k v -> Bool
isEmpty E = True
isEmpty _ = False

{-
    insert agrega un par (clave, valor) a un arbol.
-}

{-
    creaNode crea los nodes de una clave.
-}
creaNode :: Ord k => [k] -> v -> TTree k v
creaNode (c:cs) v   | cs == [] = (Leaf c v)
                    | otherwise = (Node c Nothing E (creaNode cs v) E)


insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert (c:cs) valN E = creaNode (c:cs) valN
insert (c:cs) valN (Leaf k valA)    | (k == c) && (cs == []) = (Leaf k valN)
                                    | (k == c)  = (Node k (Just valA) E (creaNode cs valN) E)
                                    | (c < k)   = (Node k (Just valA) (creaNode (c:cs) valN) E E)
                                    | otherwise = (Node k (Just valA) E E (creaNode (c:cs) valN))
insert (c:cs) valN (Node k valA l m r)  | (k == c) && (cs == []) = (Node k (Just valN) l m r)
                                        | (k == c)  = (Node k valA l (insert cs valN m) r)
                                        | (c < k)   = (Node k valA (insert (c:cs) valN l) m r)
                                        | otherwise = (Node  k valA r m (insert (c:cs) valN r))


{-
    delete elimina una clave y el valor asociado a esta.
-}
delete :: Ord k => [k] -> TTree k v -> TTree k v
