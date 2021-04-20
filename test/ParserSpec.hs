module ParserSpec
  ( testParser
  ) where
import AST
import Data.Char
import Lex
import Parse
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Data.Maybe (fromJust)

testParser :: TestTree
testParser =
  testGroup
    "Parser"
    [
        testGroup "imports" 
        [
            testCase "unaliased import" (parse "import 'a.csv' take x select a.1" @?= AST [UnaliasedImport "a.csv"] "x" [] Nothing [SelectExpr (TableColumn "a" 1)]),
            testCase "aliased import" (parse "import A 'a.csv' take x select a.1" @?= AST [AliasedImport "A" "a.csv"] "x" [] Nothing [SelectExpr (TableColumn "a" 1)]),
            testCase "multiple unaliased imports" (parse "import 'a.csv', 'b.csv' take x select a.1" @?= AST [UnaliasedImport "b.csv", UnaliasedImport "a.csv"] "x" [] Nothing [SelectExpr (TableColumn "a" 1)]),
            testCase "multiple aliased imports" (parse "import A 'a.csv', B 'b.csv' take x select a.1" @?= AST [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "x" [] Nothing [SelectExpr (TableColumn "a" 1)])
        ],
        testGroup "take" 
        [
            testCase "take identifier" (parse "import 'a.csv' take A select a.1" @?= AST [UnaliasedImport "a.csv"] "A" [] Nothing [SelectExpr (TableColumn "a" 1)])
        ],
        testGroup "join" 
        [
            testCase "no join" (parse "import A 'a.csv', B 'b.csv' take A select a.1" @?= AST [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "A" [] Nothing [SelectExpr (TableColumn "a" 1)]),
            testCase "cross join" (parse "import A 'a.csv', B 'b.csv' take A cross join B select a.1" @?= AST [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "A" [Cross "B"] Nothing [SelectExpr (TableColumn "a" 1)]),
            testCase "inner join with equals" (parse "import A 'a.csv', B 'b.csv' take A inner join B on A.1 = B.1 select a.1" @?= AST [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "A" [Inner "B" (BinaryOpExpr (TableColumn "A" 1) AST.EQ (TableColumn "B" 1))] Nothing [SelectExpr (TableColumn "a" 1)]),
            testCase "inner join with equals" (parse "import A 'a.csv', B 'b.csv', C 'c.csv' take A inner join B on A.1 = B.1, cross join C select a.1" @?= AST [AliasedImport "C" "c.csv", AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "A" [Cross "C", Inner "B" (BinaryOpExpr (TableColumn "A" 1) AST.EQ (TableColumn "B" 1))] Nothing [SelectExpr (TableColumn "a" 1)])
        ],
        testGroup "filter" 
        [
            testCase "unary operator" (parse "import A 'a.csv', B 'b.csv' take A cross join B where not 0 select a.1" @?= AST [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "A" [Cross "B"] (Just (UnaryOpExpr NOT (ValueExpr (ValueInt 0)))) [SelectExpr (TableColumn "a" 1)]),
            testCase "binary operator" (parse "import A 'a.csv', B 'b.csv' take A cross join B where a.1 = 0 select a.1" @?= AST [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "A" [Cross "B"] (Just (BinaryOpExpr (TableColumn "a" 1) AST.EQ (ValueExpr (ValueInt 0)))) [SelectExpr (TableColumn "a" 1)]),
            testCase "function" (parse "import A 'a.csv', B 'b.csv' take A cross join B where Coalesce(a.1,a.2) select a.1" @?= AST [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "A" [Cross "B"] (Just (Function "Coalesce" [TableColumn "a" 2,TableColumn "a" 1])) [SelectExpr (TableColumn "a" 1)]),
            testCase "function and unary operator" (parse "import A 'a.csv', B 'b.csv' take A cross join B where Len(a.1) = 0 select a.1" @?= AST [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "A" [Cross "B"] (Just (BinaryOpExpr (Function "Len" [TableColumn "a" 1]) AST.EQ (ValueExpr (ValueInt 0)))) [SelectExpr (TableColumn "a" 1)])
        ],
        testGroup "expression" [
            testCase "equality precedence" $ parseExpr "0+1=2" @?= "(0+1)=2"
        ],
        testGroup "select" [
            testCase "select one column" (parse "import 'a.csv' take x select a.1" @?= AST [UnaliasedImport "a.csv"] "x" [] Nothing [SelectExpr (TableColumn "a" 1)]),
            testCase "select multiple columns" (parse "import 'a.csv' take x select a.1,a.2" @?= AST [UnaliasedImport "a.csv"] "x" [] Nothing [SelectExpr (TableColumn "a" 2),SelectExpr (TableColumn "a" 1)]),
            testCase "select value" (parse "import 'a.csv' take x select 0" @?= AST [UnaliasedImport "a.csv"] "x" [] Nothing [SelectExpr (ValueExpr (ValueInt 0))]),
            testCase "select wildcard" (parse "import 'a.csv' take x select *" @?= AST [UnaliasedImport "a.csv"] "x" [] Nothing [Wildcard]),
            testCase "select qualified wildcard" (parse "import 'a.csv' take x select a.*" @?= AST [UnaliasedImport "a.csv"] "x" [] Nothing [QualifiedWildcard "a"])
        ]
    ]

parse :: String -> Query
parse = parseJCQL . alexScanTokens

parseExpr :: String -> String
parseExpr s = (init . tail) $ printExpr $ getExpr ast
    where ast = parse ("import 'a.csv' take a where "++s++" select a.1")
          getExpr (AST _ _ _ f _) = fromJust f

printExpr e = case e of
    BinaryOpExpr left op right -> "("++printExpr left++printBOP op++printExpr right++")"
    ValueExpr v -> printValue v

printBOP :: BinaryOpType -> String
printBOP op = case op of
 AST.EQ -> "="
 Sum  -> "+"
    
printValue :: Value -> String 
printValue v = case v of
    ValueInt i -> show i