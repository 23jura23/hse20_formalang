module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Char (isLower, isUpper)
import ParserAST

import Debug.Trace
import Data.Maybe

-- Language definition

languageDef =
  emptyDef { Token.identStart = lower
           , Token.identLetter = alphaNum <|> char '_'
           , Token.reservedNames = ["module", "type"]
           , Token.reservedOpNames = [",", ";", "->", ":-", "|"]
           , Token.commentStart = "(*"
           , Token.commentEnd = "*)"
           , Token.nestedComments = True
           }

lexer = Token.makeTokenParser languageDef

-- Helpful parsers --

whiteSpace = try $ Token.whiteSpace lexer
dot = Token.dot lexer
parens = Token.parens lexer
oparens s = parens s <|> s
brackets = Token.brackets lexer
turnstile = Token.reservedOp lexer ":-"
vbar = Token.reservedOp lexer "|"
comma = Token.reservedOp lexer ","

-- Binary operator (for buildExpressionParser) --
-- eta reduction of third argument
binary name fun = Infix (do { Token.reservedOp lexer name; return fun })

-- parseString --

parseString :: Parser a -> String -> Either ParseError a
parseString a =
  parse (do r <- a; eof; return r) ""

-- Program --

program = try $ do
    whiteSpace
    moduleName <- optionMaybe modul
    types      <- many typedef
    relations  <- many relation
    return (Program moduleName types relations)

-- identifier --

identifier :: Parser Identity
identifier = Id <$> Token.identifier lexer

-- Variable --

variable :: Parser Variable
variable = try $ Token.lexeme lexer $ do
    h <- letter <|> char '_'
    t <- many (alphaNum <|> char '_')
    guard $ isUpper h 
    return (Va (h:t))

-- Module --

modul :: Parser Identity
modul = try $ do
    Token.reserved lexer "module"
    i <- identifier
    dot
    return i

-- TypeDef --

typedef :: Parser TypeDef
typedef = try $ do
    Token.reserved lexer "type"
    i <- identifier
    t <- optionMaybe typ
    dot
    return (TypeDef i t)

typ :: Parser Type
typ = try $ buildExpressionParser tableType typ1
    where typ1 = parens typ <|> fmap Var variable <|> fmap TAtom atom
          tableType = [ [binary "->" Arrow AssocRight] ]

-- Relation --

relation :: Parser Relation
relation = try $ do
    head <- atom
    body <- optionMaybe (turnstile >> relationBody)
    dot
    return (Relation head body)

relationBody :: Parser RelationBody
relationBody = try $ buildExpressionParser tableRelationBody relationBody1
             where relationBody1 = parens relationBody <|> fmap RAtom atom
                   tableRelationBody = [ [binary "," Conj AssocRight], 
                                         [binary ";" Disj AssocRight] ]

-- Atom --

atom :: Parser Atom
atom = try $ list <|> liftM2 Atom identifier (many base)

atom1 :: Parser Atom
atom1 = try $ list <|> liftM2 Atom identifier (many1 base)

-- Base --

base :: Parser Base
base = try $ (parensAst (parens (fmap BaseA atom1)) <|> fmap BaseA list <|> fmap BaseI identifier <|> fmap BaseV variable) <|> parens base
    where parensAst a = try $ a <|> parens (parensAst a)

-- List --

-- additionally parses lists like [A, B | C], not only [A, B, C] and [A | B]

list :: Parser Atom
list = try $ brackets content
    where content = fromMaybe nil <$> optionMaybe (try $ do 
            l1   <- list1
            tail <- optionMaybe (vbar >> cons (BaseA l1) <$> fmap BaseV variable)
            return $ fromMaybe l1 tail)
          list1 = try $ fmap baseA $ foldr consToBaseA (BaseA nil) <$> sepBy1 base comma
          nil = Atom (Id "nil") []
          cons x y = Atom (Id "cons") [x, y]
          consToBaseA x y = BaseA $ cons x y
