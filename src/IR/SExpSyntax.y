{
module SExpSyntax where
import SExpTokens (Token(..))
}

%name parse
%tokentype {Token}
%error {parseError}

%token
'(' { LParen }
')' { RParen }
define { Define }
primop {PrimeOp $$}
sym {Symbol $$}
val {Value $$}

%%
Prog :: { Prog }
     : DefineStats Exp { Prog $1 $2 }

DefineStats :: { [DefineStat] }
        : {- Empty -} { [] }
        | DefineStats DefineStat { $2:$1 }

DefineStat :: { DefineStat }
       : '(' define sym Exp ')' { DefineStat $3 $4 }
       
Exp :: { Exp }
    : val { ValueExp $1 }
    | sym { SymbolExp $1 }
    | '(' primop Exp Exp ')' { PrimeExp $2 $3 $4 }

{
data Prog = Prog [DefineStat] Exp deriving Show

data DefineStat = DefineStat String Exp deriving Show

data Exp = ValueExp Int
         | PrimeExp Char Exp Exp
         | SymbolExp String
         deriving Show

parseError :: [Token] -> a
parseError _ = error "Parse error"
}
    
