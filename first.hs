
add a b = a + b

feng_drop n lst =
    if n <= 0 || null lst then
        lst
    else
        feng_drop (n - 1) (tail lst)


niceDrop n (x:xs)
    | n <= 0    = x:xs
    | otherwise = niceDrop (n - 1) xs
niceDrop _ [] = []


main = putStrLn (show (niceDrop 2 [1,2,3,4]))


data BookInfo = Book Int String [String]
                deriving (Show)

data BookList = BookList String [BookInfo]

data TriState = OnState | OffState | ChaocState

rotateTriState OnState = OffState
rotateTriState OffState = ChaocState
rotateTriState ChaocState = OnState
