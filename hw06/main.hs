module Main where
import Parser
import System.Environment
import System.IO
import Data.Either

main' s = case parseString s of
            Left p  -> show p
            Right e -> show e

main = do
        args <- getArgs
        case length args of
            0 -> putStr $ unlines ["Usage:", "Read from file and write to filename.out: ./main -f filename", "Read from stdin and write to stdin: ./main -s"]
            1 -> case head args of
                    "-s" -> getContents >>= putStrLn . main' 
                    _    -> putStrLn "Wrong arguments"
            2 -> case head args of
                    "-f" -> do
                            from <- openFile (args !! 1) ReadMode 
                            to <- openFile (args !! 1 ++ ".out") WriteMode
                            content <- hGetContents from
                            hPutStrLn to $ main' content
                    _    -> putStrLn "Wrong arguments"
            _ -> putStrLn "Wrong arguments"
