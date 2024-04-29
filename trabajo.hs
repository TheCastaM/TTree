{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v)
                | Leaf k v
                | E deriving Show

{-
    La estructura que se usa para crear el diccionario es un arbol
    de tres hojas donde cada Node o hoja tiene un caracter de el 
    codigo guardado en "k" y un posible valor guardado en "v" que 
    existe en el caso de que el codigo este asociado a ese valor.
-}

search [] _ = Nothing
search x E = Nothing
search (x:xs) (Leaf k v) | (xs == []) && (x == k) = Just v
                         | otherwise = Nothing
search (x:xs) (Node k v l m r) | (xs == []) && (x == k) = v
                               | (x > k) = search (x:xs) r
                               | (x < k) = search (x:xs) l
                               | otherwise = search xs m


insert :: Ord k => [k] -> v -> TTree k v -> TTree k v

insert (x:xs) val E | xs == [] = (Leaf x val)
                  | otherwise = (Node x Nothing E (insert xs val E) E)

insert (x:[]) val (Leaf k v) | x < k = (Node k (Just v) (Leaf x val) E E)
                             | x > k = (Node k (Just v) E E (Leaf x val))
                             | otherwise = (Node k (Just v) E (Leaf x val) E)

insert (x:xs) val (Leaf k v) | x < k = (Node k (Just v) (insert (x:xs) val E) E E)
                             | x > k = (Node k (Just v) E E (insert (x:xs) val E))
                             | otherwise = (Node k (Just v) E (insert xs val E) E)

insert (x:[]) val (Node k v l m r) | x < k = (Node k v (insert (x:[]) val l) m r)
                                   | x > k = (Node k v l m (insert (x:[]) val r))
                                   | otherwise = (Node k (Just val) l m r)

insert (x:xs) val (Node k v l m r) | x < k = (Node k v (insert (x:xs) val l) m r)
                                   | x > k = (Node k v l m (insert (x:xs) val r))
                                   | otherwise = (Node k v l (insert xs val m) r) 
{-
    check evalua que no haya una rama que no tenga valores.
-}
check :: Ord k => TTree k v -> TTree k v
check E = E
check (Node _ Nothing E E E) = E
check (Node _ Nothing l E E) = l
check (Node _ Nothing E E r) = r
check t = t

{-
    delete elimina una clave y el valor asociado a esta.
-}
delete :: Ord k => [k] -> TTree k v -> TTree k v
delete _ E = E
delete (c:cs) (Leaf k val)  | (c == k) && (cs == []) = E
                            | otherwise = Leaf k val
delete (c:cs) (Node k val l E r)    | (c == k) && (cs == []) = check((aux l r))
                                    | (c < k) = check((Node k val (delete (c:cs) l) E r))                                    
                                    | (c > k) = check((Node k val l E (delete (c:cs) r)))
                                    | otherwise = (Node k val l E r)
    where
        aux :: Ord k => TTree k v -> TTree k v -> TTree k v
        aux l E = l
        aux E r = r
        aux (Leaf k v) r = Node k (Just v) E E r
        aux l (Leaf k v) = Node k (Just v) l E E
        aux (Node k v l m E) r = (Node k v l m r)
        aux (Node k v l m lr) r = (Node (key(right_most lr)) 
                                            (value(right_most lr)) 
                                            (Node k v l m (righted lr))
                                            (middle_son(right_most lr)) 
                                            r)
            where
                key :: Ord k => TTree k v -> k
                key (Leaf k _) = k
                key (Node k _ _ _ _) = k
                middle_son :: Ord k => TTree k v -> TTree k v
                middle_son (Leaf _ _) = E
                middle_son (Node _ _ _ m _) = m
                value :: Ord k => TTree k v -> Maybe v
                value (Leaf _ v) = Just v
                value (Node _ v _ _ _) = v
        right_most :: Ord k => TTree k v -> TTree k v
        right_most (Leaf k v) = (Leaf k v)
        right_most (Node k v l m E) = (Node k v E m E)
        right_most (Node k v l m r) = (right_most r)
        righted :: Ord k => TTree k v -> TTree k v
        righted (Leaf _ _) = E
        righted (Node k v l m E) = l
        righted (Node k v l m r) = (Node k v l m (righted r))
delete (c:cs) (Node k val l m r)    | (c == k) && (cs == []) = check((Node k Nothing l m r))
                                    | (c == k) = check((Node k val l (delete cs m) r)) 
                                    | (c < k) = check((Node k val (delete (c:cs) l) m r))
                                    | otherwise = check((Node k val l m (delete (c:cs) r)))



{-
    keys devuelve un arreglo ordenado de las claves en arbol.
-}

keys :: Ord k => TTree k v -> [[k]]
keys E = [[]]

keys (Leaf k v) = [[k]]

keys (Node k Nothing E m E) =
    (map (\x -> k : x) (keys m))

keys (Node k v E m E) =
   [[k]] ++ (map (\x -> k : x) (keys m))

keys (Node k Nothing E m r) =
    (map (\x -> k : x) (keys m)) ++ keys r

keys (Node k v E m r) =
   [[k]] ++ (map (\x -> k : x) (keys m)) ++ keys r

keys (Node k Nothing l m E) =
   keys l ++ (map (\x -> k : x) (keys m))

keys (Node k v l m E) =
   keys l ++ [[k]] ++ (map (\x -> k : x) (keys m)) 

keys (Node k Nothing l E r) = keys(l) ++ keys r

keys (Node k v l E r) = keys(l) ++ [[k]] ++ keys r

keys (Node k Nothing l m r)  = keys(l) ++ (map (\x -> k : x) (keys m)) ++ keys r

keys (Node k v l m r) = keys(l) ++  [[k]] ++ (map (\x -> k : x) (keys m)) ++ keys r


class Dic k v d | d -> k v where
    vacio :: d
    insertar :: Ord k => k -> v -> d -> d
    buscar :: Ord k => k -> d -> Maybe v
    eliminar :: Ord k => k -> d -> d
    claves :: Ord k => d -> [k]

instance Ord k => Dic [k] v (TTree k v) where
  vacio = E
  insertar = insert
  buscar = search  
  eliminar = delete
  claves = keys

t = Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E)(Node 'o' (Just 2) (Leaf 'd' 9)E(Leaf 's' 4))E)(Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8)(Leaf 'n' 7)E)E)
