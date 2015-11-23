

import qualified Data.Char (digitToInt)
import qualified Data.Char (toUpper)


data List a =   Cons a (List a)
              | Nil
                deriving Show

fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil


safeSecond (x:y:xs) = Just y
safeSecond _ = Nothing


asInt [] = 0
asInt [x] = Data.Char.digitToInt x
asInt (x:xs) = loop (Data.Char.digitToInt x) xs

loop acc [] = acc
loop acc (x:xs) = loop ((acc * 10) + (Data.Char.digitToInt x)) xs


upperCase [] = []
upperCase (x:xs) = loop [(Data.Char.toUpper x)] (upperCase xs)
                        where loop result [] = result
                              loop result (x:xs) = loop (result ++ [(Data.Char.toUpper x)]) xs


upperCase' [] = []
upperCase' (x:xs) = (Data.Char.toUpper x) : upperCase' xs



-- main = putStrLn (show (asInt "1234"))
main = putStrLn (show (upperCase' "abcd"))
