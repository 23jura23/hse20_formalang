module ParserAST where
import Data.Maybe

data Program = Program { 
               moduleName :: Maybe Identity 
             , types      :: [TypeDef]
             , relations  :: [Relation]
             } deriving (Eq)
                       
data TypeDef = TypeDef Identity (Maybe Type)
               deriving (Eq)

data Type = Var Variable
          | TAtom Atom
          | Arrow Type Type
          deriving (Eq)

data Atom = Atom { atomHead :: Identity, atomArgs :: [Base] }
            deriving (Eq)

data Relation = Relation { relHead :: Atom, relBody :: Maybe RelationBody }
                deriving (Eq)

data RelationBody = RAtom Atom
                  | Conj RelationBody RelationBody
                  | Disj RelationBody RelationBody
                  deriving (Eq)

data Base = BaseA {baseA :: Atom} | BaseV Variable | BaseI Identity
             deriving (Eq)

newtype Variable = Va String deriving (Eq)
newtype Identity = Id String deriving (Eq)

tabRight n = unlines . map ((replicate n ' ' ++ "| ") ++ ) . lines
addBlock s t = prefix ++ tabRight n t
               where prefix = s ++ " |\n"
                     n = length prefix - 2
        
showLnR x = show x ++ "\n"
showLnL x = "\n" ++ show x

instance Show Variable where
    show (Va s) = s

instance Show Identity where
    show (Id s) = s

instance Show Base where
    show (BaseA a) = show a
    show (BaseV v) = show v
    show (BaseI i) = show i

instance Show Atom where
    show (Atom id args) = addBlock "Atom" $ showLnR id ++ concatMap showLnR args

instance Show Relation where
    show (Relation a rb) = addBlock "Relation" $ show a ++ maybe "" showLnL rb

instance Show RelationBody where
    show (RAtom a) = show a
    show (Conj rb1 rb2) = addBlock "Conjunction" $ showLnR rb1 ++ show rb2
    show (Disj rb1 rb2) = addBlock "Disjunction" $ showLnR rb1 ++ show rb2

instance Show TypeDef where
    show (TypeDef id t) = addBlock "TypeDef" $ show id ++ maybe "" showLnL t

instance Show Type where
    show (Var v) = show v
    show (TAtom a) = show a
    show (Arrow t1 t2) = addBlock "Arrow" $ showLnR t1 ++ show t2

instance Show Program where
    show (Program m ts rels) = addBlock "Program" $ maybe "" (addBlock "Module" . showLnR) m ++ addBlock "TypeDefs" (concatMap showLnR ts) ++ addBlock "Relations" (concatMap showLnR rels)
