


import System.IO
import Data.Char


mainLoop :: Handle -> Handle -> IO ()

mainLoop inFile outFile = 
    do
        inEof <- hIsEOF inFile
        if inEof then
            return ()
        else
            do
                inStr <- hGetLine inFile
                hPutStrLn outFile (map toUpper inStr)
                mainLoop inFile outFile


main = do
        inFile <- openFile "input.txt" ReadMode
        outFile <- openFile "output.txt" WriteMode

        mainLoop inFile outFile

        hClose inFile
        hClose outFile
