module Main where
import Lex
import Parse
import System.Environment
import System.IO

main' s = show $ parse $ alexScanTokens s

main = do
        args <- getArgs
        case length args of
            0 -> putStr $ unlines ["Usage:", "Read from file: ./main -f filename", "Read from stdin: ./main -s"]
            1 -> case head args of
                    "-s" -> getContents >>= putStrLn . main' 
                    _    -> putStrLn "Wrong arguments"
            2 -> case head args of
                    "-f" -> openFile (args !! 1) ReadMode >>= hGetContents >>= putStrLn . main'
                    _    -> putStrLn "Wrong arguments"
            _ -> putStrLn "Wrong arguments"
