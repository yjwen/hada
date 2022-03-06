{
module SExpTokens (Token(..), PrimeOp(..), alexScanTokens) where
import Syntax (PrimeOp(..))
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
"if" {\s -> If}
"lambda" {\s -> Lambda}
\#[tf] {\s -> BoolLiteral ((head . tail) s == 't')}
[_$alpha][$alpha$digit\_]* {\s -> Symbol s}
\-?$digit+ {\s -> IntLiteral (read s)}

{
data Token = LParen
           | RParen
           | Define
           | PrimeOp PrimeOp
           | Negate
           | If
           | Lambda
           | Symbol String
           | IntLiteral Int
           | BoolLiteral Bool
           deriving Show
}
