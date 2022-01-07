{
module SExpSyntax where
import SExpTokens (Token(..), PrimeOp(..))
}

%name parse
%tokentype {Token}
%error {parseError}

%token
'(' { LParen }
')' { RParen }
define { Define }
primop {PrimeOp $$}
negate {Negate}
sym {Symbol $$}
intlit {IntLiteral $$}
boollit {BoolLiteral $$}

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


{
data Prog = Prog [DefineStat] Exp deriving Show

data DefineStat = DefineStat String Exp deriving Show

data Exp = IntLitExp Int
         | BoolLitExp Bool
         | PrimeExp PrimeOp Exp Exp
         | NegateExp Exp
         | SymbolExp String
         deriving Show

parseError :: [Token] -> a
parseError _ = error "Parse error"
}
    
