
-- Written by Hyunyoung Lee. Copyright 2017. All rights reserved.
-- For CSCE 314 Sections 501, 502 Students For Part II of Assignment 4.
-- This file is strictly for the students of Sections 501 and 502 of CSCE314
--    in Fall 2017 to use for solely doing the Part II of Assignment 4. 
-- Sharing it with any other person or using it for any other purpose is
-- prohibited by law.

module W ( WValue(..), WExp(..), WStmt(..), exec, eval) where

 data WValue = VInt Int 
             | VBool Bool 
             | VString String -- new for Part II
             | VMarker
               deriving Eq

 instance Show WValue where
    show (VInt i)    = show i
    show (VBool b)   = show b
    show (VString s) = s
    show (VMarker)   = "_"

 data WExp = Val WValue
           | Var String
               
           | Plus WExp WExp
           | Mult WExp WExp
               
           | Equals      WExp WExp
           | NotEqual    WExp WExp
           | LessOrEq    WExp WExp
           | Less        WExp WExp
    
           | And  WExp WExp
           | Or   WExp WExp
           | Not  WExp
             deriving Show

 data WStmt = Empty 
            | VarDecl String WExp
            | Assign  String WExp
            | If    WExp WStmt WStmt
            | While WExp WStmt
            | Block [WStmt]
            | Print WExp   -- new for Part II
              deriving Show

 type Memory = [(String, WValue)]
 marker = ("|", VMarker)
 isMarker (x, _) = x == "|"

 -- some useful helper functions
 asInt (VInt v) = v
 asInt x = error $ "Expected a number, got " ++ show x
    
 asBool (VBool v) = v
 asBool x = error $ "Expected a boolean, got " ++ show x

 ----------
 -- eval --
 ----------

 eval :: WExp -> Memory -> WValue
    
 eval (Val v) _ = v
   
 eval (Var s) m = 
      case lookup s m of
          Nothing -> error $ 
                      "Unknown variable " ++ s ++ " in memory " ++ show m
          Just v -> v
    
 eval (Plus e1 e2) m =
      let e1' = eval e1 m
          e2' = eval e2 m
      in VInt $ asInt e1' + asInt e2'
 eval (Mult e1 e2) m =
      let e1' = eval e1 m
          e2' = eval e2 m
      in VInt $ asInt e1' * asInt e2'
    
 eval (Equals e1 e2) m =
      let e1' = eval e1 m
          e2' = eval e2 m
      in VBool $ e1' == e2'
 eval (NotEqual e1 e2) m = VBool $ not $ asBool $ eval (Equals e1 e2) m
    
 eval (Less e1 e2) m =
      let e1' = eval e1 m
          e2' = eval e2 m
      in VBool $ asInt e1' < asInt e2'
 eval (LessOrEq e1 e2) m =
      let e1' = eval e1 m
          e2' = eval e2 m
      in VBool $ asInt e1' <= asInt e2'
    
 eval (And e1 e2) m | not (asBool (eval e1 m)) = VBool False
                    | otherwise = VBool (asBool (eval e2 m))
 eval (Or e1 e2) m  | asBool (eval e1 m) = VBool True
                    | otherwise = VBool (asBool (eval e2 m))
 eval (Not e) m = VBool $ not $ asBool $ eval e m
    
 ----------
 -- exec --
 ----------
 exec :: WStmt -> Memory -> IO Memory
 exec Empty m = return m
 exec (VarDecl s e) m 
              | not $ definedInThisScope s m = return $ (s, eval e m) : m
              | otherwise = error $ 
                           "Var " ++ s ++ " already defined in this scope"
      where definedInThisScope s [] = False
            definedInThisScope s (x@(k,_):xs) 
                             | isMarker x = False
                             | s == k     = True
                             | otherwise  = definedInThisScope s xs

 exec (Assign s e) m = return $ replaceFirstDefFound s (eval e m) m
   where replaceFirstDefFound _ _ [] = error $ 
                                       "Cannot assign undefined var " ++ s
         replaceFirstDefFound s v (x@(k,_):xs)
            | s == k = if sameKind v x then (k,v):xs
                                       else error "Type error in assignment"
            | otherwise = x: replaceFirstDefFound s v xs
            where sameKind v@(VInt _) x@(_, VInt _) = True
                  sameKind v@(VBool _) x@(_, VBool _) = True
                  sameKind v@(VString _) x@(_, VString _) = True
                  sameKind _ _ = False

 exec (If e s1 s2) m | eval e m == VBool True = exec s1 m
                     | eval e m == VBool False = exec s2 m
                     | otherwise = error "Non-boolean in condition of if"

 exec (While e s) m | eval e m == VBool True = exec s m >>= \m' ->
                                               exec (While e s) m'  
                    | eval e m == VBool False = return m
                    | otherwise = error "Non-boolean in condition of while"

 exec (Block ss) m = exec' ss (marker:m) >>= \m' -> return $ popMarker m'
   where exec' [] m = return m
         exec' (s:ss) m = exec s m >>= \m' -> exec' ss m' 
         popMarker [] = []
         popMarker (x:xs) | isMarker x = xs
                          | otherwise  = popMarker xs

 exec (Print e) m = putStr (show (eval e m)) >> return m

-- example programs
 prog1 = Block
   [
     VarDecl "x" (Val (VInt 0)),
     VarDecl "y" (Val (VInt 1)),
     VarDecl "b" (Less (Val (VInt 0)) (Var "x")),
     If (Or (Var "b") (LessOrEq (Var "x") (Val (VInt 0))) )
        ( Block [ Assign "x" (Val (VInt 1)),
                  Assign "y" (Plus (Var "y") (Val (VInt 1)))
                ] 
        )
        ( Assign "x" (Val (VInt 2)) )
  ]

 factorial = Block
  [
     VarDecl "acc" (Val (VInt 1)),
     While (Less (Val (VInt 0)) (Var "arg") )
     ( Block
       [ Assign "acc" (Mult (Var "acc") (Var "arg")),
         Assign "arg" (Plus (Var "arg") (Val (VInt (-1))))         
       ]
     ),
     Assign "result" (Var "acc")
  ]


-- to test the example programs
-- once you load this file to ghci

-- exec prog1 []
-- must return []

-- exec factorial [("result", VInt (-1)), ("arg", VInt 10)]
-- must return [("result",3628800),("arg",0)]
