import Data.List

opponent x =
    case x of
        'O' -> 'X'
        'X' -> 'O'

in_board (i, j) = 1 <= i && i <= 6 && 1 <= j && j <= 7

get board (i, j) = if in_board (i, j) then board !! (i - 1) !! (j - 1) else '#'

moves board = [j | j <- [1..7], get board (1, j) == '.']

update_row row mark = init (takeWhile (== '.') row) ++ [mark] ++ dropWhile (== '.') row

show_board board = 
    do
        mapM_ putStrLn [intersperse ' ' (i : j) | (i, j) <- zip ['1'..'7'] board]
        putStrLn ("  " ++ (intersperse ' ' ['1'..'7']))
        
------------------------------------------------------------------------------------------
--transpose board = [[i !! j | i <- board] | j <- [0..length (head board) - 1]]

update_board board row mark = transpose (take (row - 1) (transpose board) ++ [update_row ((transpose board) !! (row - 1)) mark] ++ drop row (transpose board))

fours (x, y) = 
    filter (all in_board) 
    ([[(x, i) | i <- [y .. y + 3]]] 
        ++ [[(i, y) | i <- [x .. x + 3]]] 
        ++ [[(i, j) | (i, j) <- zip [x .. x + 3] [y .. y + 3]]] 
        ++ [[(i, j) | (i,j) <- zip [x, x - 1 .. x - 3] [y .. y + 3]]])

fours2 (x,y) = filter f
    ([[(x,i) | i <- [y .. y + 3], in_board (x,i)]]
    ++ [[(i,y) | i <- [x .. x + 3], in_board (i,y)]]
    ++ [[t | t <- zip [x .. x + 3] [y .. y + 3], in_board t]]
    ++ [[t | t <- zip [x, x - 1 .. x - 3] [y .. y + 3], in_board t]])
        where
            f x = 4 == length x


win board mark = or (map f [(i, j) | i <- [1..6], j <- [1..7]])
    where
        f point = any f2 (fours point)
        f2 rist = all f3 rist
        f3 taple = mark == (get board taple)

ttt = 
    do
        let board = [".......", ".......", ".......", ".......", ".......", "......."]
        mapM_ putStrLn ["----- 四目並べ -----", "6行7列の四目並べです",　"", "ルール", "1.列を指定してください", "2.先行のコマはOです", "3.縦横斜めに4つコマを先に並べた方が勝ちとなります", "4.すべてのマスが埋まってしまったら引き分けとなります", ""]
        game board 'O'
            where
                game board mark =
                    do
                        show_board board
                        if win board (opponent mark)
                            then 
                                putStrLn ("Winner : " ++ [opponent mark])
                            else if moves board == []
                                then
                                    putStrLn "draw!!"
                            else        
                                do
                                    putStrLn ""
                                    putStrLn (mark : "のターンです")
                                    putStr ("コマを置く列を指定してください(置ける列　" ++ show (moves board) ++ ") : ")
                                    row <- getLine
                                    if row `elem` (map show [1..7])
                                        then
                                            do
                                                if (read row :: Int) `elem` (moves board)
                                                    then
                                                        game (update_board board (read row :: Int) mark) (opponent mark)
                                                    else
                                                        sequence_ [putStrLn "error!! : 置ける列を指定してください", game board mark]
                                        else
                                            sequence_ [putStrLn "error!! 列を指定してください", game board mark]
