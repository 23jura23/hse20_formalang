module Main where
import Lex
import Parse
import System.Environment
import System.IO

removeComments :: [Token] -> Maybe [Token]
removeComments = removeComments' 0
               where removeComments' :: Int -> [Token] -> Maybe [Token]
                     removeComments' n []         | n == 0 = Just []
                                                  | otherwise = Nothing
                     removeComments' n ((Lc _):t) | n >= 0 = removeComments' (n+1) t
                                                  | otherwise = Nothing
                     removeComments' n ((Rc _):t) | n > 0 = removeComments' (n-1) t
                                                  | otherwise = Nothing
                     removeComments' n (h:t)      | n > 0  = removeComments' n t
                                                  | n == 0 = fmap (h:) (removeComments' n t)
                                                  | otherwise = Nothing

main' s = case removeComments $ alexScanTokens s of
                (Just tokens) -> show $ parse tokens
                Nothing -> "Unmatched multiline comments bracket"

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
