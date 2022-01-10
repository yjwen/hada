{
module SExpSyntax where
import SExpTokens (Token(..), PrimeOp(..))
import Syntax
}

%name parse
%tokentype {Token}
%error {parseError}

%token
'(' { LParen }
')' { RParen }
define { Define }
if { If }
primop {PrimeOp $$}
negate {Negate}
sym {Symbol $$}
intlit {IntLiteral $$}
boollit {BoolLiteral $$}
lambda {Lambda}

%%
Prog :: { Prog }
     : DefineStats Exp { Prog $1 $2 }

DefineStats :: { [DefineStat] }
        : {- Empty -} { [] }
        | DefineStats DefineStat { $2:$1 }

DefineStat :: { DefineStat }
       : '(' define sym Exp ')' { DefineStat $3 $4 }
       
Exp :: { Exp }
    : intlit { IntLitExp $1 }
    | boollit { BoolLitExp $1 }
    | '(' primop Exp Exp ')' { PrimeExp $2 $3 $4 }
    | '(' negate Exp ')' { NegateExp $3 }
    | sym { SymbolExp $1 }
    | '(' if Exp Exp Exp ')' {IfExp $3 $4 $5}
    | '(' lambda sym Exp ')' {LambdaExp $3 $4}
    | '(' Exp Exp ')' {AppExp $2 $3}


{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
    
