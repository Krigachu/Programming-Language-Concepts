import Lexing
import Parsing
import Evaluator
import System.Environment
import Control.Exception
import System.IO

main :: IO String
main = catch main' noParse

main2 = do (fileName : _ ) <- getArgs 
           sourceText <- readFile fileName
           putStrLn ("Parsing : " ++ sourceText)
           let parsedProg = parseCalc (alexScanTokens sourceText)
           putStrLn ("Parsed as " ++ (show parsedProg))

--(fileName : "<" : inputFile ) <- getArgs 

main' = do (fileName : _) <- getArgs 
           sourceText <- readFile fileName
           --testText <- readFile testFile
           --putStrLn ("Parsing : " ++ sourceText)
           --print (alexScanTokens sourceText)
           --print (parseCalc (alexScanTokens sourceText))
           --let result = eval (parseCalc (alexScanTokens sourceText))
           retrieveLine <- getLine
           --retrieveLine <- testText !! 0
           --let retrieveLine = getLine
           --inputList <- loopFile testText 0 
           inputList <- loopFile retrieveLine
--           inputList <- return (loopFile inputFile 0 emptyString)
           inputVar <- getStreamNo retrieveLine
           --inputVar <- getStreamNo (testText !! 0)
           doProgram ((parseCalc (alexScanTokens sourceText)), [("StreamVar", inputVar)], [("Stream", inputList)])



--loopFile ::  String -> Int -> IO [Int]
--loopFile f lineNo = do if null (f !! lineNo) 
--                       then return([])
 --                      else do 
--                        list1 <- loopFile f (lineNo + 1)
--                        return( (map (read::String->Int) (words (f !! lineNo))) ++ list1 )

loopFile :: String -> IO [Int]
loopFile l = do e <- isEOF
                if e
                then return(map (read::String->Int) (words (l)))
                else do
                  g <- getLine
                  list1 <- loopFile g
                  return( (map (read::String->Int) (words (l))) ++ list1 )

getStreamNo :: String -> IO Int
getStreamNo l = do return (length( map (read::String->Int) (words l)))

--getStreamNo f = return (length( map (read::String->Int) (words (readFile (f !! 0)))))

       --    do file <- openFile "text.txt" ReadWriteMode 
         -- line1 <- hGetLine file
          --let lineInput = words line1
          --print (map (read::String->Int) lineInput)
          --line2 <- hGetLine file
          --print line2


noParse :: ErrorCall -> IO String
noParse e = do let err =  show e
               hPutStr stderr err
               return ("")


