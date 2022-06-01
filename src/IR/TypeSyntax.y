{
module TypeSyntax where
import TypeTokens(Token(..))
import Type
}

%name parse
%tokentype { Token }
%error { parseError }

%token

'(' { LParen }
')' { RParen }
int{ IntToken }
bool { BoolToken }
any { AnyToken }
arrow { ArrowToken }

%%

Type :: { Type }
     : int { TyInt }
     | bool { TyBool }
     | any { TyAny }
     | Type arrow Type { TyArrow $1 $3 }
     | '(' Type ')' { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
