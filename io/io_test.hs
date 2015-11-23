


nameToReplay :: String -> String

nameToReplay name =
  "Pleased to meet you, " ++ name ++ ".\n"


main = do
          putStrLn "Greetings!"
          inputStr <- getLine
          putStrLn ("Welcome " ++ inputStr)

          let outStr = nameToReplay inputStr
          putStrLn outStr
