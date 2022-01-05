{
module SExpTokens (Token(..), alexScanTokens) where
}

%wrapper "basic"

$alpha = [a-zA-Z]
$digit = [0-9]

tokens :-

$white+ ;
\;.*    ;
\( {\s -> LParen}
\) {\s -> RParen}
[\+\-\*] {\s -> PrimeOp (head s)}
"define" {\s -> Define}
[_$alpha][$alpha$digit\_]* {\s -> Symbol s}
$digit+ {\s -> Value (read s)}

{
data Token = LParen
           | RParen
           | Define
           | PrimeOp Char
           | Symbol String
           | Value Int
           deriving Show
}
