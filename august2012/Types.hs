module Types where

import Data.List

data Ast = 
   Var String
 | And Ast Ast
 | Or Ast Ast
 | Not Ast
 | Impl Ast Ast
 | Equi Ast Ast  deriving (Eq, Show)

data LogicVal = Good | HZ | Bad deriving (Eq, Show)
  
getVariables :: Ast -> [String]
getVariables x = 
  helper x
  where 
    helper x = case x of
        Var x -> [x]
        And x y  -> helper x ++ helper y
        Or x y   -> helper x ++ helper y
        Impl x y -> helper x ++ helper y
        Equi x y -> helper x ++ helper y
        Not x    -> helper x

evalAnd :: LogicVal -> LogicVal -> LogicVal 
evalAnd Good Good = Good
evalAnd ____ HZ   = HZ
evalAnd HZ   ____ = HZ
evalAnd Bad  ____ = Bad
evalAnd ____ Bad  = Bad

evalOr :: LogicVal -> LogicVal -> LogicVal 
evalOr Good ____ = Good
evalOr ____ Good = Good
evalOr HZ   ____ = HZ
evalOr ____ HZ   = HZ
evalOr Bad  Bad  = Bad

evalImpl :: LogicVal -> LogicVal -> LogicVal 
evalImpl HZ   ____ = Good
evalImpl Bad  ____ = Good
evalImpl Good Good = Good
evalImpl Good HZ   = HZ
evalImpl Good Bad  = Bad

evalEq :: LogicVal -> LogicVal -> LogicVal 
evalEq x y | x == y = Good
evalEq HZ  _____    = HZ
evalEq ____ HZ      = HZ
evalEq ____ _______ = Bad

evalNot :: LogicVal -> LogicVal
evalNot Good  =  Bad
evalNot HZ    = HZ
evalNot Bad   = Good


eval :: Ast -> (String -> LogicVal) -> LogicVal
eval x f = 
  helper x
  where 
    helper x = case x of
        Var x -> f x
        And  x y -> evalAnd (helper x) (helper y)
        Or   x y -> evalOr  (helper x) (helper y)
        Impl x y -> evalImpl(helper x) (helper y)
        Not  x   -> evalNot (helper x)
        Equi x y -> evalEq  (helper x) (helper y)

        