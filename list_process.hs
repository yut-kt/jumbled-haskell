fromto x y = [x..y]


indices x y = filter f [0..length y - 1]
    where f t = y !! t == x


values x y = map snd (filter f x)
    where f t = fst t == y


rotate x y = if y < 0 then drop (length x - mod (abs y) (length x)) x ++ take (length x - mod (abs y) (length x)) x
                else drop (mod y (length x)) x ++ take (mod y (length x)) x


caesar x y = 
    case y of
    [] -> []
    (' ':ys) -> ' ' : caesar x ys
    (y:ys) -> map snd (filter f (zip ['a'..'z'] (rotate ['a'..'z'] x))) ++ caesar x ys
            where f t = (fst t) == y

