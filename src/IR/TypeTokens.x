{
module TypeTokens (Token(..), alexScanTokens) where
import Type(Type(..))
}

%wrapper "basic"

tokens :-

$white+ ;
\( {\s -> LParen}
\) {\s -> RParen}
"int" {\s -> IntToken}
"bool" {\s -> BoolToken}
"any" {\s -> AnyToken}
"->" {\s -> ArrowToken}
{
data Token = LParen
           | RParen
           | IntToken
           | BoolToken
           | AnyToken
           | ArrowToken
           deriving Show
}
