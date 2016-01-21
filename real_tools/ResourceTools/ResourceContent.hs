

module ResourceContent
(
    isStringReferedByList
,   fileContentsInPaths
)


where


import Data.List (isInfixOf)

import System.IO (openFile,
                  IOMode (..))

import GHC.IO.Handle

import Control.DeepSeq (deepseq)
import Control.Exception (try,
                          IOException)



isStringReferedByList :: String -> [String] -> Bool

isStringReferedByList _ []     = False
isStringReferedByList s [x]    = isInfixOf s x
isStringReferedByList s (x:xs) = (isInfixOf s x) || (isStringReferedByList s xs)



fileContentInOnePath :: String -> IO String

fileContentInOnePath path =
        do
            handle <- openFile path ReadMode
            content <- hGetContents handle
            result <- try (content `deepseq` hClose handle) :: IO (Either IOException ())
            case result of
                Left _ -> return ""
                Right _ -> return content
            


fileContentsInPaths :: [String] -> IO [String]

fileContentsInPaths [path]      = do
                                    content <- fileContentInOnePath path
                                    return [content]

fileContentsInPaths []          = return []

fileContentsInPaths (path:xs)   = do
                                    content <- fileContentInOnePath path
                                    contents <- fileContentsInPaths xs
                                    return (content : contents)

