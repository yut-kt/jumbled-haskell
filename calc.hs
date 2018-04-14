chi [] [] = 0.0
chi (x:xs) (y:ys) = (x - y) ^^ 2 / y + chi xs ys

chi2 x y = sum [(i - j) ^^ 2 / j | i <- x, j<- y ,indiceshead i x == indiceshead j y]


glength x = fromIntegral (length [i | i <- x, i /= ' '])
count x y = glength [i | i <- x, i == y]
freq x = map (/ glength x) [count x i | i <- ['a'..'z']]


expect = [0.082, 0.015, 0.028, 0.043, 0.127, 0.022, 0.020,
            0.061, 0.070, 0.002, 0.008, 0.040, 0.024, 0.067,
            0.075, 0.019, 0.001, 0.060, 0.063, 0.091, 0.028,
            0.010, 0.024, 0.002, 0.020, 0.001]
nearest x = minimum [(chi (freq i) expect, i) | i <- x]




rotate x y = if y < 0 then drop (length x - mod (abs y) (length x)) x ++ take (length x - mod (abs y) (length x)) x
                else drop (mod y (length x)) x ++ take (mod y (length x)) x
caesar x y = 
    case y of
    [] -> []
    (' ':ys) -> ' ' : caesar x ys
    (y:ys) -> map snd (filter f (zip ['a'..'z'] (rotate ['a'..'z'] x))) ++ caesar x ys
            where f t = (fst t) == y
-----------------------------------
indiceshead x y = head (filter f [0..length y - 1])
    where f t = y !! t == x
nearestsnd x = snd (nearest x)
caesartable x = [caesar (-i) x | i <- [1..26]]
decode x = ((+1) (indiceshead (nearestsnd (caesartable x)) (caesartable x)), nearestsnd (caesartable x))

nearest2 x = minimum [(chi (freq i) expect, (j, i)) | (i, j) <- zip x [1..26]]

decode2 x = snd (nearest2 (caesartable x))
{--decode x = ((+1) (head (indices (snd (nearest [caesar (-i) x | i <- [1..26]])) [caesar (-i) x | i <- [1..26]])), snd (nearest [caesar (-i) x | i <- [1..26]]))-}
