module Main where
import Text.ParserCombinators.Parsec
import Parser
import System.Environment
import System.IO
import Debug.Trace

showFromEither (Left  e) = show e
showFromEither (Right e) = show e

main' :: String -> String -> String
main' p s = case p of
                "--atom"     -> showFromEither $ parseString atom     s
                "--typeexpr" -> showFromEither $ parseString typ      s
                "--module"   -> showFromEither $ parseString modul    s
                "--relation" -> showFromEither $ parseString relation s
                "--list"     -> showFromEither $ parseString list     s
                "--prog"     -> showFromEither $ parseString program  s
                _            -> "Parser " ++ s ++ " not found"
    
main = do
        args <- getArgs
        case length args of
            0 -> putStr $ unlines ["Usage:", "Use selected parser, then read from file and write to filename.out: ./main parser filename", "Read from stdin and write to stdin using Program parser: ./main -s", "Available parsers:", "Atom: \t\t--atom", "Type: \t\t--typexpr", "Module: \t--module", "Relation: \t--relation", "List: \t\t--list", "Program: \t--prog"]
            1 -> case head args of
                    "-s" -> getContents >>= putStrLn . main' "--prog" 
                    _    -> putStrLn "Wrong arguments"
            2 -> do
                    content <- readFile (args !! 1)
                    writeFile (args !! 1 ++ ".out") $ main' (args !! 0) content
            _ -> putStrLn "Wrong arguments"
