
-- Assignment 4 Part II, CSCE-314 Sections 501 and 502
-- Instructor: Hyunyoung Lee
-- Section: 501
-- Student Name: Jonathan Smith, Pengbo Ma, Nick Tillerson
-- UIN: 324004796, 824005703, 123003843
-- (Acknowledge any help received here)


module WParser ( parse, wprogram ) where

import Data.Char
import Control.Monad
import Control.Applicative
import W

-------------------
-- keywords of W --
-------------------

keywords = words "var if else while print true false"
isKeyword s = s `elem` keywords

keyword s = do s' <- identifier
               if s' == s then return s else empty

variable = do s <- identifier
              if isKeyword s then empty else return (Var s)

-----------------------------
-- This is the main parser --
-----------------------------
wprogram = space >> many stmt >>= \ss -> return ( Block ss )

-- a program is a sequence of statements; the parser returns them
-- as a single block-statement

-- parser functions for some of the statement types below are
-- already defined, and the rest are undefined.
-- your task is to implement them

stmt = varDeclStmt <|> assignStmt <|> ifStmt <|> whileStmt <|>
       blockStmt <|> emptyStmt <|> printStmt

emptyStmt = symbol ";" >> return Empty

printStmt = do
   keyword "print"
   e <- expr
   symbol ";"
   return $ Print e

varDeclStmt = do
   keyword "var"
   s <- identifier
   symbol "="
   e <- expr
   symbol ";"
   return $ VarDecl s e

assignStmt = do
  --  keyword "assign"
    s <- identifier
    symbol "="
    e <- expr
    symbol ";"
    return $ Assign s e

ifStmt = do
    keyword "if"
    cond <- paren expr
    s1 <- stmt
    keyword "else"
    s2 <- stmt
    return $ If cond s1 s2

whileStmt = do
    keyword "while"
    cond <- paren expr
    s <- stmt
    return $ While cond s

blockStmt = do
    symbol "{"
    stmts <- many stmt
    symbol "}"
    return $ Block stmts


-- the only kind of expression supported for now is stringLiterals
-- implement the full expression language of W (part of them is
-- provided in the assignment 4 statement (Section 4.5.2 of hw4.pdf file)
expr' = stringLiteral

expr = logicExpr >>= logicExprSeq
logicExprSeq left =
    ( do op <- (symbol "&&" >> return And) <|> (symbol "||" >> return Or)
         right <- logicExpr
         logicExprSeq (op left right)
    ) <|> return left

logicExpr =
    ( do
    symbol "!"
    logicExpr >>= \x -> return (Not x)
    )<|> comparisonExpr



comparisonExpr = arithmeticExpr >>= comparisonExprSeq
comparisonExprSeq left =
    ( do op <- (symbol "==" >> return Equals) <|> (symbol "!=" >> return NotEqual) <|> (symbol "<=" >> return LessOrEq) <|> (symbol "<" >> return Less)
         right <- arithmeticExpr
         comparisonExprSeq (op left right)
    ) <|> return left

arithmeticExpr = term >>= termSeq
termSeq left =
    ( do op <- (symbol "+" >> return Plus)
         right <- term
         termSeq (op left right)
    ) <|> return left

term = factor >>= factorSeq
factorSeq left =
    ( do op <- (symbol "*" >> return Mult)
         right <- factor
         factorSeq (op left right)
    ) <|> return left

factor = (integer >>= \n -> return $ Val (VInt n)) <|> variable <|> stringLiteral <|> paren expr <|> bool

-- Bool literals
bool = (keyword "true"  >> return (Val (VBool True))) <|>
       (keyword "false" >> return (Val (VBool False)))

-- stringLiterals can contain '\n' characters
stringLiteral = do char ('"')
                   s <- many stringChar
                   char ('"')
                   space
                   return $ Val (VString s)

stringChar = do ( char '\\' >> char 'n' >> return '\n' )
                <|> sat (/= '"')

----------------------
-- Parser utilities --
----------------------

-- Basic definitions

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     [] -> []
                     (x:xs) -> [(x, xs)])

-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   return = pure
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

   -- many and some are already defined in the Alternative class:
   -- many :: Parser a -> Parser [a]
   -- many p = some p <|> pure []
   -- some :: Parser a -> Parser [a]
   -- some p = pure (:) <*> p <*> many p


-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit, letter, lower, alphanum :: Parser Char
digit    = sat isDigit
letter   = sat isAlpha
lower    = sat isLower
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- Useful building blocks
ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           comment
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

comment = do
    string "//"
    many (sat (/= '\n'))
    space
    <|> return ()

paren p = do
    symbol "("
    p' <- p
    symbol ")"
    return p'
