

module ResourceName
(
    stripFactor
,   showListInLines
)


where



stripFactor :: String -> String

stripFactor ['@', _ , 'x']  = ""
stripFactor (x:xs)          = x : stripFactor xs
stripFactor []              = ""



showListInLines :: [String] -> String

showListInLines [] = []
showListInLines [a, b] = a ++ "\n" ++ b
showListInLines (x : xs) = x ++ "\n" ++ showListInLines xs



