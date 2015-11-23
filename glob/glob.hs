

module Glob
(
    namesMatching
)
where


import Text.Regex.Posix
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))

import Control.Exception (handle)
import Control.Exception.Base (IOException)
import Control.Monad (forM)
import GlobRegex (matchesGlob)



isPattern :: String -> Bool
isPattern = any (`elem` "[*?")



doesNameExists :: FilePath -> IO Bool

doesNameExists name = do
    fileExists <- doesFileExist name
    if fileExists
        then return True
        else doesDirectoryExist name



listMatches :: FilePath -> String -> IO [String]

listMatches dirName pat =
          let
                handler :: IOException -> IO [String]
                handler = const (return [])
                useCurrentDir = ((null dirName) || ("./" == dirName))
          in do
                dirName' <- (if useCurrentDir then getCurrentDirectory
                                              else return dirName)
                handle handler
                       (do
                            names <- getDirectoryContents dirName'
                            let names' = if (isHidden pat) then
                                            filter isHidden names else
                                            filter (not . isHidden) names
                            return (filter (`matchesGlob` pat) names'))



listPlain :: FilePath -> String -> IO [String]

listPlain dirName name =
    do
        exists <- if (null name) then
                      doesDirectoryExist dirName else
                      doesNameExists (dirName </> name)
        return (if exists then [name] else [])



isHidden :: String -> Bool

isHidden ('.' : _) = True
isHidden _ = False



namesMatching :: String -> IO [String]

namesMatching pat
    | not (isPattern pat) =
            do
                exists <- doesNameExists pat
                return (if exists then [pat] else [])
    | otherwise =
            case (splitFileName pat) of
                ("./", baseName) ->
                    do
                        curDir <- getCurrentDirectory
                        listMatches curDir pat
                (dirName, baseName) ->
                    do
                        dirs <- if isPattern dirName then
                                    namesMatching (dropTrailingPathSeparator dirName) else
                                    return [dirName]
                        let listDir = if (isPattern baseName) then
                                          listMatches else
                                          listPlain
                        let listPathNames dir =
                                    do
                                        baseNames <- listDir dir baseName
                                        return (map (dir </>) baseNames)
                        pathNames <- forM dirs listPathNames
                        return (concat pathNames)



-- main = do
--     namesMatching "."

