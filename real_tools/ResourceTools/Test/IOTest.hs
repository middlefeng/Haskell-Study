
module IOTest where


funcIO :: String -> IO String

funcIO a = do
    return a


funcTakeLast :: [a] -> Maybe a


funcTakeLast [a, b] = Just b
funcTakeLast [] = Nothing
funcTakeLast (_ : xs) = funcTakeLast xs



ioConcat :: String -> String -> IO String

ioConcat s1 s2 = do
                aStr <- funcIO s1
                bStr <- funcIO s2
                return (aStr ++ bStr)
