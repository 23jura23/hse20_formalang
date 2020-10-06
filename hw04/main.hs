module Main where
import Data.Maybe
import Text.Printf
import System.Environment
import System.IO
import Lex

-- G -> L | LG
-- L -> W :- B. | W.
-- B -> K;B | K
-- K -> T,K | T
-- T -> W | (B)
-- W # Word from lexer

getLexemError :: [Token] -> Maybe Token
getLexemError []             = Nothing
getLexemError (Error w p:ts) = Just $ Error w p
getLexemError (_:ts)         = getLexemError ts

parseG :: [Token] -> (Bool, [Token])
parseG [] = (True, [])
parseG ts = case parseL ts of
                 (True, ts') -> parseG ts'
                 e           -> e

parseL :: [Token] -> (Bool, [Token])
parseL [] = (True, [])
parseL (Word _ _:Dot _:t) = parseG t
parseL (Word _ _:Turnstile _:ts) = case parseB ts of
                                        (True, Dot _:ts') -> (True, ts')
                                        (True, ts')         -> (False, ts')
                                        e                   -> e
parseL t = (False, t)

parseB :: [Token] -> (Bool, [Token])
parseB [] = (False, [])
parseB ts = case parseK ts of
                 (True, Semicolon _:ts') -> parseB ts'
                 (True, ts')                -> (True, ts')
                 e                          -> e

parseK :: [Token] -> (Bool, [Token])
parseK [] = (False, [])
parseK ts = case parseT ts of
                 (True, Comma _:ts') -> parseK ts'
                 (True, ts')            -> (True, ts')
                 e                      -> e

parseT :: [Token] -> (Bool, [Token])
parseT (Word _ _:ts) = (True, ts)
parseT (Lb _:ts)     = case parseB ts of
                            (True, Rb _:ts') -> (True, ts')
                            (True, ts')       -> (False, ts')
                            e                 -> e
parseT ts            = (False, ts)                           

parseError :: Token -> String
parseError e = printf "Syntax error at word \"%s\": line %d, colon %d"  w l c
               where (w, (l, c)) = case e of
                                      Turnstile p -> (":-", getPos p)
                                      Comma     p -> ("," , getPos p)
                                      Semicolon p -> (";" , getPos p)
                                      Dot       p -> ("." , getPos p)
                                      Lb        p -> ("(" , getPos p)
                                      Rb        p -> (")" , getPos p)
                                      Word    w p -> (w   , getPos p)
                                      Error   w p -> (w   , getPos p)
                                   where getPos (AlexPn _ l c) = (l, c)

main' s = let tokens = alexScanTokens s in
             case getLexemError tokens of
                 Just e  -> parseError e
                 Nothing -> case parseG tokens of
                                 (True, []) -> "Correct syntax"
                                 (False, ts) -> printf "Incorrect syntax. %s" e
                                                 where e = case ts of [] -> "Tokens list is empty. Perhaps some lexem is missing"
                                                                      (t:_) -> parseError t
  

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
