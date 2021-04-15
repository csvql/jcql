{
module Parse where
import Lex
import AST
}

%name parseJCQL 
%tokentype { Token } 
%error { parseError }
%token
    import {TKeyword "import" _}
    take {TKeyword "take" _}
    join {TKeyword "join" _}
    inner {TKeyword "inner" _}
    cross {TKeyword "cross" _}
    on {TKeyword "on" _}
    case {TKeyword "case" _}
    when {TKeyword "when" _}
    then {TKeyword "then" _}
    else {TKeyword "else" _}
    and {TKeyword "and" _}
    => {TArrow _}
    = {TOperator "=" _}
    == {TOperator "==" _}
    - {TOperator "-" _}
    + {TOperator "+" _}
    / {TOperator "/" _}
    ^ {TOperator "^" _}
    % {TOperator "%" _}
    compare {TOperator "compare" _}
    not {TOperator "not" _}
    and {TOperator "and" _}
    or {TOperator "or" _}
    ( {TBracket "(" _}
    ) {TBracket ")" _}
    , {TComma _}
    . {TDot _}
    string {TString $$ _}
    int {TInt $$ _}
    true {TBool True _}
    false {TBool False _}
    identifier {TIdentifier $$ _}

%%

ast : import many(importExpr) {AST $1 [] [] [] []}
importExpr : TIdentifier TString {AliasedImport $1 $2}
    | TString {UnaliasedImport $1}


many(p,s) : p            { [$1] }
          | many(p,s) s space p  { $4:$1 }

any(p,s) :   { [] }
         | p            { [$1] }
         | many(p,s) s space p { $4:$1 }

opt(p) : p { Just $1 }
      |   { Nothing }
