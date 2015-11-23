

module GlobRegex
(
    globToRegex,
    matchesGlob
)
where


import Text.Regex.Posix


globToRegex :: String -> String

globToRegex cs = '^' : globToRegex' cs ++ "$"




globToRegex' :: String -> String

globToRegex' "" = ""
globToRegex' ('*' : cs) = '.' : '*' : globToRegex' cs
globToRegex' ('?' : cs) = '.' : globToRegex' cs
globToRegex' ('[' : '!' : c : cs) = '[' : '^' : c : charClass cs
globToRegex' ('[' : c : cs) = '[' : c : charClass cs
globToRegex' ('[' : _) = error "undetermined"

globToRegex' (c:cs) = (escape c) ++ (globToRegex' cs)




charClass :: String -> String

charClass (ls@(']'):cs) = ls : globToRegex' cs
charClass (c:cs) = c : charClass cs
charClass [] = error "undetermined"



escape :: Char -> String

escape c | (elem c regexChars) = '\\' : [c]
         | otherwise = [c]
         where regexChars = "\\+()^$.{}]|"



matchesGlob :: FilePath -> String -> Bool

matchesGlob name pat = name =~ (globToRegex pat)

