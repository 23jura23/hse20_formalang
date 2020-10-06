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

Expr : w '.'              { Expr (Head $ HId $ word $1) }
     | w Head '.'         { Expr (HId (word $1) :@ $2)  }
     | w ts Disj '.'      { Head (HId $ word $1) :- $3  }
     | w Head ts Disj '.' { HId (word $1) :@ $2 :- $4   }

Head : HId              { Head $1  }
     | HId Head         { $1 :@ $2 }

HId  : w                { HId (word $1)             }
     | '(' w Head ')'   { HBr (HId (word $2) :@ $3) }

Disj : Conj             { Disj $1   }
     | Conj ';' Disj    { $1 :| $3  }

Conj : Atom             { Conj $1   }
     | Atom ',' Conj    { $1 :& $3  }

Atom : Id               { Atom $1   }
     | Id Atom          { $1 :@@ $2 }

Id   : w                { Id (word $1) }
     | '(' Disj ')'     { Br $2        }

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

data Expr = Expr Head
          | Head :- Disj

data Head = Head HId 
          | HId :@ Head

data HId  = HId String
          | HBr Head

data Disj = Disj Conj
          | Conj :| Disj

data Conj = Conj Atom
          | Atom :& Conj

data Atom = Atom Id
          | Id :@@ Atom

data Id   = Id String
          | Br Disj 

tabRight n = unlines . map ((replicate n ' ' ++ "| ") ++ ) . lines
addBlock s t = prefix ++ (tabRight n t)
               where prefix = s ++ " |\n"
                     n = (length prefix) - 2

instance Show Program where
    show t = addBlock "Program" $ show' t
        where show' (Program t) = show t
              show' (e :~ t) = show e ++ "\n" ++ show' t

instance Show Expr where
    show (Expr head)    = show head
    show (head :- body) = addBlock "Turnstile" $ show head ++ "\n" ++ show body

instance Show Head where
    show s = addBlock "Head" $ show' s
             where show' (Head s) = show s
                   show' (s :@ ss) = show s ++ "\n" ++ show' ss

instance Show HId where
    show (HId s) = s
    show (HBr d) = addBlock "Brackets" $ "Left bracket" ++ "\n\n" ++ show d ++ "\n\n" ++ "Right bracket"

instance Show Disj where
    show s = addBlock "Body" $ show' s
        where show' (Disj s) = show s
              show' s = addBlock "Disjunction" $ show'' s
                   where show'' (Disj s) = show s
                         show'' (s :| ss) = show s ++ "\n" ++ show'' ss

instance Show Conj where
    show (Conj s) = show s
    show s        = addBlock "Conjunction" $ show' s
                    where show' (Conj s) = show s
                          show' (s :& ss) = show s ++ "\n" ++ show' ss

instance Show Atom where
    show (Atom s) = show s
    show s        = addBlock "Atom" $ show' s
                    where show' (Atom s) = show s
                          show' (s :@@ ss) = show s ++ "\n" ++ show' ss

instance Show Id where
    show (Id s) = s
    show (Br d) = addBlock "Brackets" $ "Left bracket" ++ "\n\n" ++ show d ++ "\n\n" ++ "Right bracket"
 
}
