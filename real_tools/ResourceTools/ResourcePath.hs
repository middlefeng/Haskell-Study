

module ResourcePath
(
    getFilesInPath
,   getFilesInList
,   lastComponent
,   stripExtension
)


where



import System.Directory (doesDirectoryExist,
                         doesFileExist,
                         getDirectoryContents)

import Data.Text (breakOnEnd, pack, unpack)



isPseudoPath :: String -> Bool

isPseudoPath "." = True
isPseudoPath ".." = True
isPseudoPath _ = False


appendToPath :: String -> String -> String

appendToPath path component = path ++ "/" ++ component



breakStringOnEnd :: String -> String -> (String, String)

breakStringOnEnd d s = let (base, ext) = (breakOnEnd d' s')
                                where d' = pack d
                                      s' = pack s
                            in ((unpack base), (unpack ext))




lastComponent :: String -> String

lastComponent path = let (_, component) = breakStringOnEnd "/" path in
                        component




stripExtension :: String -> String

stripExtension name = let (base', ext') = breakStringOnEnd "." name in
                        case base' of
                            "" -> ext'
                            _  -> stripLastDot base'
                          where
                            stripLastDot :: String -> String
                            stripLastDot [a, '.'] = [a]
                            stripLastDot [] = []
                            stripLastDot (x : xs) = x : (stripLastDot xs)





getFilesInPath :: String -> IO [String]

getFilesInPath path = do
                        directoryExists <- doesDirectoryExist path
                        if directoryExists
                            then getFilesInDirectory path
                            else do
                                fileExists <- doesFileExist path
                                if fileExists
                                    then return [path]
                                    else return []



getFilesInDirectory :: String -> IO [String]

getFilesInDirectory dircotryPath = do
        currentLevel <- (getDirectoryContents dircotryPath)
        getFilesInList $ fmap (appendToPath dircotryPath) (filter (not . isPseudoPath) currentLevel)





getFilesInList :: [String] -> IO [String]

getFilesInList []       = return []

getFilesInList [path]   = getFilesInPath path

getFilesInList (path : xs) = do
    files <- getFilesInList xs
    filesNextLevel <- getFilesInPath path
    return (filesNextLevel ++ files)


