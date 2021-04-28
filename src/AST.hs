module AST where

import           Data.Char
import           Data.List
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


printExpr e = case e of
  TableColumn name idx -> name ++ "." ++ show (idx + 1)
  ValueExpr v         -> printValue v
  BinaryOpExpr left op right ->
    "(" ++ printExpr left ++ " " ++ printBOP op ++ " " ++ printExpr right ++ ")"
  UnaryOpExpr op expr -> "(" ++ printUOP op ++ " " ++ printExpr expr ++ ")"
  Function name args ->
    name ++ "(" ++ intercalate ", " (map printExpr args) ++ ")"
  Case whens els ->
    "case "
      ++ unwords (map printWhen whens)
      ++ ") else ("
      ++ printExpr els
      ++ ") end"

printWhen (condition, value) =
  "when (" ++ printExpr condition ++ ") then (" ++ printExpr value

printBOP :: BinaryOpType -> String
printBOP op = case op of
  AST.EQ   -> "="
  AST.LT -> "<"
  AST.LEQ -> "<="
  AST.GT -> ">"
  AST.GEQ -> ">="
  AND      -> "and"
  OR       -> "or"
  Sum      -> "+"
  Difference -> "-"
  Product  -> "*"
  Division -> "/" -- TODO: we actually doing division?

printUOP :: UnaryOpType -> [Char]
printUOP op = case op of
  NOT -> "not"

printValue :: Value -> String
printValue v = case v of
  ValueInt  i -> show i
  ValueBool b -> map toLower $ show b
  ValueString s -> "'"++s++"'"

printValueType :: Value -> String
printValueType v = case v of
  ValueInt _ -> "integer"
  ValueBool _ -> "boolean"
  ValueString _ -> "string"
