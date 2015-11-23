

pluralise :: String -> [Int] -> [String]


pluralise word counts = map plural counts
                            where plural 0 = "no " ++ word ++ "s"
                                  plural 1 = "one " ++ word
                                  plural n = (show n) ++ " " ++ word ++ "s"



pluralise' word counts = let plural 0 = "no' " ++ word ++ "s"
                             plural 1 = "one " ++ word
                             plural n = (show n) ++ " " ++ word ++ "s" in
                            map plural counts


splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs in
        pre : case suf of ('\r':'\n':rest) -> splitLines rest
                          ('\r':rest)      -> splitLines rest
                          ('\n':rest)      -> splitLines rest
                          _                -> []

isLineTerminator '\r' = True
isLineTerminator '\n' = True
isLineTerminator _ = False


-- main = putStrLn (show (pluralise' "a-name" [0, 1, 2, 3]))
main = putStrLn (show (splitLines "a\r\nb\nc"))
