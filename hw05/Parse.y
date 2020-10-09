{
module Parse where
import Lex
import Text.Printf
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    w           { Word _    _ }
    ts          { Turnstile _ }
    '.'         { Dot       _ }
    ';'         { Semicolon _ }
    ','         { Comma     _ }
    '('         { Lb        _ }
    ')'         { Rb        _ }
%%

Program : Expr             { Program $1 }
        | Expr Program     { $1 :~ $2   }

Expr : Atom '.'         { Expr $1 }
     | Atom ts BrAst(Disj) '.' { $1 :- $3  }

Disj : BrAst(Conj)             { Disj $1   }
     | BrAst(Conj) ';' AddBr(Disj)    { $1 :| $3  }

Conj : BrAst(Atom)             { Conj $1   }
     | BrAst(Atom) ',' AddBr(Conj)    { $1 :& $3  }

Atom : Id             { Atom $1  }
     | Id AddBr(Atom)   { $1 :@ $2 }

AddBr(t) : BrAst(t)        { AddBr $1 }
         | BrAst(t) AddBr(t) { $1 :@@ $2 }

BrAst(t)  : t                { NoBr $1}
          | '(' BrAst(t) ')' {   Br $2}

Id   : w                { Id (word $1) }

{

parseError :: [Token] -> a
parseError []    = error "Some lexems are missing"           
parseError (e:_) = error $ printf "Syntax error at word \"%s\": line %d, colon %d"  w l c
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

data Program = Program Expr
             | Expr :~ Program
             deriving Show

data Expr = Expr Atom
          | Atom :- BrAst Disj
          deriving Show

data Disj = Disj (BrAst Conj)
          | BrAst Conj :| AddBr Disj
          deriving Show

data Conj = Conj (BrAst Atom)
          | BrAst Atom :& AddBr Conj
          deriving Show

data Atom = Atom Id 
          | Id :@ AddBr Atom
          deriving Show

data AddBr t = AddBr (BrAst t)
             | BrAst t :@@ AddBr t
             deriving Show

-- data BrAtom = BrAtom (BrAst Atom)
--             | BrAst Atom :@@ BrAtom
--             deriving Show

data Id  = Id String
         deriving Show

data BrAst t = NoBr t
             | Br (BrAst t)
             deriving Show

tabRight n = unlines . map ((replicate n ' ' ++ "| ") ++ ) . lines
addBlock s t = prefix ++ (tabRight n t)
               where prefix = s ++ " |\n"
                     n = (length prefix) - 2

-- instance Show Program where
--     show t = addBlock "Program" $ show' t
--         where show' (Program t) = show t
--               show' (e :~ t) = show e ++ "\n" ++ show' t
-- 
-- instance Show Expr where
--     show (Expr head)    = show head
--     show (head :- body) = addBlock "Turnstile" $ show head ++ "\n" ++ show body
-- 
-- instance Show Head where
--     show s = addBlock "Head" $ show' s
--              where show' (Head s) = show s
--                    show' (s :@ ss) = show s ++ "\n" ++ show' ss
-- 
-- instance Show HId where
--     show (HId s) = s
--     show (HBr d) = addBlock "Brackets" $ "Left bracket" ++ "\n\n" ++ show d ++ "\n\n" ++ "Right bracket"
-- 
-- instance Show Disj where
--     show s = addBlock "Body" $ show' s
--         where show' (Disj s) = show s
--               show' s = addBlock "Disjunction" $ show'' s
--                    where show'' (Disj s) = show s
--                          show'' (s :| ss) = show s ++ "\n" ++ show'' ss
-- 
-- instance Show Conj where
--     show (Conj s) = show s
--     show s        = addBlock "Conjunction" $ show' s
--                     where show' (Conj s) = show s
--                           show' (s :& ss) = show s ++ "\n" ++ show' ss
-- 
-- instance Show Atom where
--     show (Atom s) = show s
--     show s        = addBlock "Atom" $ show' s
--                     where show' (Atom s) = show s
--                           show' (s :@ ss) = show s ++ "\n" ++ show' ss
-- 
-- instance Show Id where
--     show (Id s) = s
--     show (Br d) = addBlock "Brackets" $ "Left bracket" ++ "\n\n" ++ show d ++ "\n\n" ++ "Right bracket"
 
}
