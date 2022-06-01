module Syntax ( PrimeOp(..), Prog(..), DefineStat(..), Exp(..)
              , reducible) where

data PrimeOp = Plus | Minus | Times | LessThan | Equal | And | Or
             deriving Show

data Prog = Prog { progDefs :: [DefineStat]
                 , progExp :: Exp
                 } deriving Show

data DefineStat = DefineStat { defSym :: String
                             , defExp :: Exp
                             } deriving Show

data Exp = IntLitExp Int
         | BoolLitExp Bool
         | PrimeExp PrimeOp Exp Exp
         | NegateExp Exp
         | SymbolExp String
         | IfExp Exp Exp Exp
         | LambdaExp String Exp
         | AppExp Exp Exp
         deriving Show


reducible :: Exp -> Bool
reducible e = case e of
                IntLitExp _ -> False
                BoolLitExp _ -> False
                LambdaExp _ _ -> False
                otherwise -> True
