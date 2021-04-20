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
    end {TKeyword "end" _}
    where {TKeyword "where" _}
    select {TKeyword "select" _}
    '=' {TOperator "=" _}
    '==' {TOperator "==" _}
    '-' {TOperator "-" _}
    '+' {TOperator "+" _}
    '/' {TOperator "/" _}
    '^' {TOperator "^" _}
    '%' {TOperator "%" _}
    '*' {TAsterisk _}
    '>' {TOperator ">" _}
    '<' {TOperator "<" _}
    '>=' {TOperator ">=" _}
    '<=' {TOperator "<=" _}
    not {TOperator "not" _}
    or {TOperator "or" _}
    and {TOperator "and" _}
    '(' {TBracket "(" _}
    ')' {TBracket ")" _}
    ',' {TComma _}
    '.' {TDot _}
    string {TString $$ _}
    int {TInt $$ _}
    true {TBool True _}
    false {TBool False _}
    identifier {TIdentifier $$ _}

%nonassoc '=' '<' '>' '<=' '>='
%right '*' '/' '^' '%'
%right '+' '-'
%right not
%right and
%right or
%nonassoc true false
%%

ast : import many(importExpr, ',') take identifier any(joinExpr, ',') filter select many(selectItems,',') {AST $2 $4 $5 $6 $8}

importExpr : identifier string {AliasedImport $1 $2}
    | string {UnaliasedImport $1}

joinExpr : cross join identifier {Cross $3}
    | inner join identifier on expr {Inner $3 $5}

filter : where expr {Just $2}
    |   {Nothing}

selectItems : '*' {Wildcard}
    | identifier '.' '*' {QualifiedWildcard $1}
    | expr {SelectExpr $1}

expr : atom binaryOp expr {BinaryOpExpr $1 $2 $3}
    | expr0 {$1}

expr0 : unaryOp expr {UnaryOpExpr $1 $2}
    | atom {$1}

atom : identifier '.' int {TableColumn $1 $3}
    | value {ValueExpr $1}
    | identifier '(' many(expr,',') ')' {Function $1 $3}

value : string {ValueString $1}
    | int {ValueInt $1}

binaryOp : '=' {AST.EQ}
    |'+' {Sum}
    | '-' {Difference}
    | and {AND}
    | or {OR}
    | '>' {AST.GT}
    | '<' {AST.LT}
    | '>=' {GEQ}
    | '<=' {LEQ}
    | '*' {Product}

unaryOp : not {NOT}

many(p,s) : p            { [$1] }
    | many(p,s) s p  { $3:$1 }

opt(p) : p { Just $1 }
    |   { Nothing }

any(p,s) :   { [] }
    | p            { [$1] }
    | many(p,s) s p { $3:$1 }
{

parseError :: [Token] -> a
parseError tokens = error $ "Parse error at token: " ++ show token ++ "\nAt position: "
      where
            token = head tokens
}