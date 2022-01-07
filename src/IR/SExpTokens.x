{
module SExpTokens (Token(..), PrimeOp(..), alexScanTokens) where
}

%wrapper "basic"

$alpha = [a-zA-Z]
$digit = [0-9]

tokens :-

$white+ ;
\;.*    ;
\( {\s -> LParen}
\) {\s -> RParen}
\+ {\s -> PrimeOp Plus}
\- {\s -> PrimeOp Minus}
\* {\s -> PrimeOp Times}
\< {\s -> PrimeOp LessThan}
"eq" {\s -> PrimeOp Equal}
"not" {\s -> Negate}
"and" {\s -> PrimeOp And}
"or" {\s -> PrimeOp Or}
"define" {\s -> Define}
\#[tf] {\s -> BoolLiteral ((head . tail) s == 't')}
[_$alpha][$alpha$digit\_]* {\s -> Symbol s}
$digit+ {\s -> IntLiteral (read s)}

{
data PrimeOp = Plus | Minus | Times | LessThan | Equal | And | Or
             deriving Show
data Token = LParen
           | RParen
           | Define
           | PrimeOp PrimeOp
           | Negate
           | Symbol String
           | IntLiteral Int
           | BoolLiteral Bool
           deriving Show
}
