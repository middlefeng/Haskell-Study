
data NewInt = N Int deriving (Eq)

newtype NewTypeInt = NT Int deriving (Eq)


main = putStrLn (show (NT 1 == NT 1))
