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
     | Atom ts Disj '.' { $1 :- $3  }

Disj : Conj             { Disj $1   }
     | Conj ';' Disj    { $1 :| $3  }

Conj : Base             { Conj $1   }
     | Base ',' Conj    { $1 :& $3  }

Base : Atom             { Base1 $1 }
     | Br(Disj)         { Base2 $1 }

Atom : Id               { Atom $1  }
     | Atom ABase       { $1 :@ $2 }

ABase : Id              { ABase1 $1 }
      | BrAst(Br(Atom)) { ABase2 $1 }

Br(t) : '(' t ')' { Br $2}

BrAst(t)  : t                { NoBrAst $1}
          | '(' BrAst(t) ')' { BrAst $2 }   -- Happy fails to build parser if it is Br(BrAst(t)) instead

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

data Expr = Expr Atom
          | Atom :- Disj

data Disj = Disj Conj
          | Conj :| Disj

data Conj = Conj Base 
          | Base :& Conj

data Base = Base1 Atom
          | Base2 (Br Disj)

data Atom = Atom Id 
          | Atom :@ ABase

data ABase = ABase1 Id
           | ABase2 (BrAst (Br Atom))

data Id = Id String

data Br t = Br t

data BrAst t = NoBrAst t
             | BrAst (BrAst t)

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

instance Show Disj where
    show (Disj s) = show s
    show s        = addBlock "Disjunction" $ show' s
                    where show' (Disj s) = show s
                          show' (s :| ss) = show s ++ "\n" ++ show' ss

instance Show Conj where
    show (Conj s) = show s
    show s        = addBlock "Conjunction" $ show' s
                    where show' (Conj s) = show s
                          show' (s :& ss) = show s ++ "\n" ++ show' ss

instance Show Base where
    show (Base1 s) = show s
    show (Base2 s) = show s

instance Show Atom where
    show (Atom s) = show s
    show s        = addBlock "Atom" $ show' s
                    where show' (Atom s) = show s
                          show' (s :@ ss) = show' s ++ "\n" ++ show ss

instance Show ABase where
    show (ABase1 s) = show s
    show (ABase2 s) = show s

instance Show Id where
    show (Id s) = s

instance Show t => Show (Br t) where
    show (Br d) = addBlock "Brackets" $ "Left bracket" ++ "\n\n" ++ show d ++ "\n\n" ++ "Right bracket"
 
instance Show t => Show (BrAst t) where
    show (NoBrAst t) = show t
    show (BrAst t)   = show (Br t)

}
