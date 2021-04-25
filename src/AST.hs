module AST where

data Query = AST [Import] TableQuery-- import (...)
  deriving (Show, Eq)

type TableQuery
  = (Identifier, -- take {{identifier}}
                 [Join], -- join (...)
                         Filter, -- filter 
                                 Select) -- select

data Import =
  AliasedImport Identifier Location
  | UnaliasedImport Location
    deriving (Show, Eq)

data TableValue =
  TableRef Identifier
  | InlineTable TableQuery
  deriving (Show, Eq)

-- TODO: Think about whether we need any more joins than this 
data Join =
  Inner TableValue Expr
  | Cross TableValue
    deriving (Show, Eq)

type Location = String
type Identifier = String
type Filter = Maybe Expr -- Best to have maybe in case there is no filter
type Select = [SelectItem]

data SelectItem =
  SelectExpr Expr
  | QualifiedWildcard Identifier
  | Wildcard
    deriving (Show, Eq)

data Expr =
  TableColumn String Int-- identifier, such as table name
  | ValueExpr Value -- literal value
  | BinaryOpExpr Expr BinaryOpType Expr
  | UnaryOpExpr UnaryOpType Expr
  | Function String [Expr] -- e.g. COALESCE(p.1, q.1)
  | Case [(Expr, Expr)] Expr -- CASE statement, https://www.w3schools.com/sql/sql_case.asp
    deriving (Show,Eq)

data Value =
    ValueString String
    | ValueInt Int
    | ValueBool Bool
    deriving (Show,Eq)

data BinaryOpType =
  EQ
  | LT -- less than, <
  | LEQ -- less than or equal to, <=
  | GT -- greater than, >
  | GEQ -- greater than or equal to, >=
  | AND
  | OR
  | Sum -- +
  | Difference -- -
  | Product -- multiply, *
  | Division
  deriving (Show,Eq)

data UnaryOpType = NOT
  deriving (Show, Eq)
