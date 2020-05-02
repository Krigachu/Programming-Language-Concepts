import Lexing
import Parsing
import Evaluator
import System.Environment
import Control.Exception
import System.IO

main :: IO ()
main = catch main' noParse

main2 = do (fileName : _ ) <- getArgs 
           sourceText <- readFile fileName
           putStrLn ("Parsing : " ++ sourceText)
           let parsedProg = parseCalc (alexScanTokens sourceText)
           putStrLn ("Parsed as " ++ (show parsedProg))

main' = do (fileName : _ ) <- getArgs 
           sourceText <- readFile fileName
           putStrLn ("Parsing : " ++ sourceText)
           print (alexScanTokens sourceText)
           print (parseCalc (alexScanTokens sourceText))

           
noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()


