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

%left or
%left and
%nonassoc '=' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%right not
%%

ast : imports rootTableQuery {AST $1 $2}
    
imports : import many(importExpr, ',') {$2}
        | {[]}
    
rootTableQuery: take identifier any(joinExpr, ',') filter selectItems { ($2,$3,$4,$5) }

tableQuery: identifier {TableRef $1}
 | '(' rootTableQuery ')' {InlineTable $2}

importExpr : identifier string {AliasedImport $1 $2}
    | string {UnaliasedImport $1}

joinExpr : cross join tableQuery {Cross $3}
    | inner join tableQuery on expr {Inner $3 $5}

filter : where expr {Just $2}
    |   {Nothing}

selectItems : select many(selectItem,',') {$2}
            | {[]}

selectItem : '*' {Wildcard}
    | identifier '.' '*' {QualifiedWildcard $1}
    | expr {SelectExpr $1}

expr : expr '=' expr { BinaryOpExpr $1 AST.EQ $3 }
    | expr '+' expr { BinaryOpExpr $1 Sum $3 }
    | expr '-' expr { BinaryOpExpr $1 Difference $3 }
    | expr and expr { BinaryOpExpr $1 AND $3 }
    | expr or expr { BinaryOpExpr $1 OR $3 }
    | expr '<' expr { BinaryOpExpr $1 AST.LT $3 }
    | expr '>' expr { BinaryOpExpr $1 AST.GT $3 }
    | expr '<=' expr { BinaryOpExpr $1 LEQ $3 }
    | expr '>=' expr { BinaryOpExpr $1 GEQ $3 }
    | expr '*' expr { BinaryOpExpr $1 Product $3 }
    | expr '/' expr { BinaryOpExpr $1 Division $3 }
    | not expr {UnaryOpExpr NOT $2}
    | expr0 {$1}

expr0 : case whenthens else expr end {Case $2 $4}
    | atom {$1}
    
whenthens: whenthen { [$1] }
         | whenthens whenthen { $1++[$2] }
whenthen : when expr then expr { ($2, $4) }

atom : identifier '.' int {TableColumn $1 ($3-1)}
    | value {ValueExpr $1}
    | '(' expr ')' {$2}
    | true { ValueExpr (ValueBool True) }
    | false { ValueExpr (ValueBool False) }
    | identifier '(' many(expr,',') ')' {Function $1 $3}

value : string {ValueString $1}
    | int {ValueInt $1}

unaryOp : not {NOT}

many(p,s) : p            { [$1] }
    | many(p,s) s p  { $1++[$3] }

opt(p) : p { Just $1 }
    |   { Nothing }

any(p,s) :   { [] }
    | p            { [$1] }
    | many(p,s) s p { $3:$1 }
{

parseError :: [Token] -> a
parseError tokens = error $ "unexpected token "++token
      where
            token = if null tokens then "incomplete expression" else tokenPosn (head tokens)
}
