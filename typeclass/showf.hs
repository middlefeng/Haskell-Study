



{-
module Showf
(
    Showf,
--    intList
)
where
-}



--showObj :: a -> String

--showObj [o]




class Showf a where

    showF        :: a -> String

    showListF    :: [a] -> String
    showListF list =
        "[" ++ showItem list ++ "]"





showItem :: (Showf a) => [a] -> String

showItem [x] = (showF x)
showItem (x:xs) = (showF x) ++ ", " ++ (showItem xs)
showItem [] = ""


instance Showf Int where

    showF = show



-- intList :: [Int]
-- intList = [1,2,3]



instance Showf Char where
    
    showF x = showListF [x]

    showListF (x:xs) = x:xs
    showListF [] = ""



instance Showf a => Showf [a] where
    
    showF xs =  showListF xs





main :: IO ()

main = do
        putStrLn (showF "abc")
        return ()
