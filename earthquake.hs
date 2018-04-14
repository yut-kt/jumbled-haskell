num_records file =
    do
        x <- readFile file
        print (length [i | i <- lines x, 'J' == head i])


except x = 'J' == head x && elem (x !! 52) ['0'..'9'] && elem (x !! 53) ['0'..'9']
m_to_d x = 0.1 * read [x !! 52, x !! 53] :: Double

num_records_p n file =
    do
        x <- readFile file
        print (length [i | i <- lines x, except i, n <= m_to_d i])


count x = fromIntegral (length [i | i <- lines x, except i])
stat file =
    do
        x <- readFile file
        let av = (sum [m_to_d i | i <- lines x, except i]) / count x
        let sd = sqrt ((sum [((m_to_d i) - av) ^^ 2 | i <- lines x, except i]) / count x)
        print (av, sd)


hist file =
    do
        x <- readFile file
        print (map length ([[k | j <- lines x, except j, let k = m_to_d j, i == (floor k)] | i <- [0..9]]))
