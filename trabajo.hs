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
                                        | otherwise = (Node  k valA l m (insert (c:cs) valN r))

{-
    key devuelve el codigo de un nodo
-}
key :: Ord k => TTree k v -> k
key (Leaf k _) = k
key (Node k _ _ _ _) = k

{-
    value devuelve el valor de un nodo
-}
value :: Ord k => TTree k v -> Maybe v
value (Leaf _ v) = Just v
value (Node _ v _ _ _) = v

{-
    middle_son devuelve el hijo del medio
-}
middle_son :: Ord k => TTree k v -> TTree k v
middle_son E = E
middle_son (Leaf _ _) = E
middle_son (Node _ _ _ m _) = m

check :: Ord k => TTree k v -> TTree k v
check E = E
check (Node _ Nothing E E E) = E
check t = t

{-
    delete elimina una clave y el valor asociado a esta.
-}
delete :: Ord k => [k] -> TTree k v -> TTree k v
delete _ E = E
delete (c:cs) (Leaf k val)  | (c == k) && (cs == []) = E
                            | otherwise = Leaf k val
delete (c:cs) (Node k val l E E)    | (c == k) && (cs == []) = l
                                    | (c < k) = check((Node k val (delete (c:cs) l) E E))
                                    | otherwise = (Node k val l E E)
delete (c:cs) (Node k val E E r)    | (c == k) && (cs == []) = r
                                    | (c > k) = check((Node k val E E (delete (c:cs) r)))
                                    | otherwise = (Node k val E E r)
delete (c:cs) (Node k val l E r)    | (c == k) && (cs == []) = check((aux l r))
                                    | (c < k) = check((Node k val (delete (c:cs) l) E r))                                    
                                    | (c > k) = check((Node k val l E (delete (c:cs) r)))
                                    | otherwise = (Node k val l E r)
    where
        aux :: Ord k => TTree k v -> TTree k v -> TTree k v
        aux (Leaf k v) r = Node k (Just v) E E r
        aux l (Leaf k v) = Node k (Just v) l E E
        aux (Node k lv ll lm lr) r = (Node (key(right_most lr)) 
                                            (value(right_most lr)) 
                                            (Node k lv ll lm (righted lr))
                                            (middle_son(right_most lr)) 
                                            r)
        right_most :: Ord k => TTree k v -> TTree k v
        right_most (Leaf k v) = (Leaf k v)
        right_most (Node k v l m E) = (Node k v E m E)
        right_most (Node k v l m r) = (right_most r)
        righted :: Ord k => TTree k v -> TTree k v
        righted (Leaf _ _) = E
        righted (Node k v l m (Leaf _ _)) = (Node k v l m E)
        righted (Node k v l m (Node _ _ l1 _ E)) = (Node k v l m l1)
        righted (Node k v l m r) = (Node k v l m (righted r))
delete (c:cs) (Node k val l m r)    | (c == k) && (cs == []) = check((Node k Nothing l m r))
                                    | (c == k) = check((Node k val l (delete cs m) r)) 
                                    | (c < k) = check((Node k val (delete (c:cs) l) m r))
                                    | (c > k) = check((Node k val l m (delete (c:cs) r)))
                                    | otherwise = (Node k val l m E) 
    
t = Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E)(Node 'o' (Just 2) (Leaf 'd' 9)E(Leaf 's' 4))E)(Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8)(Leaf 'n' 7)E)E)
tree = insert "rea" 1 t
tree1 = insert "reda" 2 tree
tree2 = insert "ree" 3 tree1
tree3 = insert "rez" 4 tree2
tree4 = insert "resa" 5 tree3
ftree = insert "rer" 5 tree4