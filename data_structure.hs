data Day =
    Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Eq, Show)

data Signal =
    Blue | Yellow | Red
    deriving (Eq, Show)

next x =
    case x of
        Blue -> Yellow
        Yellow -> Red
        Red -> Blue

data Shape =
    Circle Double | Rect Double Double
    deriving (Eq, Show)
