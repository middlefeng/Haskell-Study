



import System.Environment

import Data.List (isInfixOf)

import ResourcePath (getFilesInList,
                     lastComponent,
                     stripExtension)

import ResourceName (stripFactor, showListInLines)



hasOriginalFactor :: String -> [String] -> Bool


hasOriginalFactor name [path]   = name == path
hasOriginalFactor _ []          = False
hasOriginalFactor name (x:xs)   = hasOriginalFactor name [x] || hasOriginalFactor name xs






hasScaleFactor :: String -> String -> [String] -> Bool

hasScaleFactor name factor list = let nameWithFactor = name ++ "@" ++ factor ++ "x" in
                                        case filter (isInfixOf nameWithFactor) list of
                                            [] -> False
                                            _  -> True



removeDuplication :: [String] -> [String]

removeDuplication s = accuUniqueSet s [] where
                        accuUniqueSet []  accum     = accum
                        accuUniqueSet (x:xs) accum 
                                    | elem x accum  = accuUniqueSet xs accum
                                    | otherwise     = x : accuUniqueSet xs accum




removeNothing :: [Maybe String] -> [String]

removeNothing []              = []
removeNothing [Nothing]       = []
removeNothing [Just x]        = [x]
removeNothing (Nothing:xs)    = removeNothing xs
removeNothing (Just x : xs)   = x : removeNothing xs





checkMissingFactors :: String -> [String] -> (String, [String])

checkMissingFactors name list =
    let oneX = if hasOriginalFactor name list then Nothing else Just "1x"
        twoX = if hasScaleFactor name "2" list then Nothing else Just "2x"
        threeX = if hasScaleFactor name "3" list then Nothing else Just "3x"
    in
        (name, removeNothing [oneX, twoX, threeX])





checkAllMissingFactors :: [String] -> [String] -> [(String, [String])]

checkAllMissingFactors names list = 
    removeFullFactorItem
        (fmap (\name -> checkMissingFactors name list) names)





removeFullFactorItem :: [(String, [String])] -> [(String, [String])]


removeFullFactorItem []             = []
removeFullFactorItem ((_, []):xs)   = removeFullFactorItem xs
removeFullFactorItem (x:xs)         = x : removeFullFactorItem xs




showMissingItem :: (String, [String]) -> String

showMissingItem (name, missedFactor) = name ++ ", " ++ show missedFactor




main :: IO ()

main = do
            args <- getArgs
            resourcePath <- getFilesInList args
            let resourceNames =
                    (removeDuplication . (fmap (stripFactor . stripExtension . lastComponent))) resourcePath
            let missingFactors =
                    checkAllMissingFactors resourceNames (fmap (stripExtension . lastComponent) resourcePath)
            putStrLn $ showListInLines (fmap showMissingItem missingFactors)
                                            
            
