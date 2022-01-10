module Syntax (PrimeOp(..), Prog(..), DefineStat(..), Exp(..)) where

data PrimeOp = Plus | Minus | Times | LessThan | Equal | And | Or
             deriving Show

data Prog = Prog [DefineStat] Exp deriving Show

data DefineStat = DefineStat String Exp deriving Show

data Exp = IntLitExp Int
         | BoolLitExp Bool
         | PrimeExp PrimeOp Exp Exp
         | NegateExp Exp
         | SymbolExp String
         | IfExp Exp Exp Exp
         | LambdaExp String Exp
         | AppExp Exp Exp
         deriving Show

