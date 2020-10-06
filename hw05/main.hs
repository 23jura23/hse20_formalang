module Main where
import Lex
import Parse

main = do s <- getContents
          print $ parse $ alexScanTokens s
