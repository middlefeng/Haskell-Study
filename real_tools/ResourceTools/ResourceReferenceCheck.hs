



import System.Environment

import ResourcePath (getFilesInList,
                     lastComponent,
                     stripExtension)

import ResourceContent (fileContentsInPaths,
                        isStringReferedByList)

import ResourceName (stripFactor, showListInLines)



separateArgs :: [String] -> (Maybe [String], Maybe String)

separateArgs [] = (Nothing, Nothing)

separateArgs list@[_] = (Just list, Nothing)

separateArgs [a, b] = (Just [a], Just b)

separateArgs (x : xs)  = let (xs', last') = separateArgs xs in
                            case xs' of
                                Just list -> (Just (x : list), last')
                                Nothing   -> (Nothing, Nothing)




resourceShortName :: String -> String

resourceShortName = stripFactor . stripExtension . lastComponent




type ResourcePath = String
type SroucePath = String

orphanResource :: [ResourcePath] -> [SroucePath] -> [String]

orphanResource resources sources = 
    fmap resourceShortName (filter (\str -> not (isStringReferedByList (resourceShortName str) sources)) resources)




main :: IO ()

main = do
            args <- getArgs
            let (sources, resrouces) = separateArgs args in
                case resrouces of
                    Nothing -> error "Too few argument"
                    Just rsc ->
                        case sources of
                            Nothing -> error "Invalid argument"
                            Just src -> do
                                            sourcePaths <- getFilesInList src
                                            resourcePath <- getFilesInList [rsc]
                                            fileContents <- fileContentsInPaths sourcePaths
                                            let orphans = orphanResource resourcePath fileContents
                                            putStrLn $ showListInLines orphans
                                            
            
