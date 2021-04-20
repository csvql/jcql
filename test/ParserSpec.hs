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
            testCase "Parses unaliased import" (parse "import 'a.csv' take x" @?= AST [UnaliasedImport "a.csv"] "x" [] Nothing []),
            testCase "Parses aliased import" (parse "import A 'a.csv' take x" @?= AST [AliasedImport "A" "a.csv"] "x" [] Nothing []),
            testCase "Parses multiple unaliased imports" (parse "import 'a.csv', 'b.csv' take x" @?= AST [UnaliasedImport "b.csv", UnaliasedImport "a.csv"] "x" [] Nothing []),
            testCase "Parses multiple aliased imports" (parse "import A 'a.csv', B 'b.csv' take x" @?= AST [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "x" [] Nothing [])
        ],
        testGroup "take" 
        [
            testCase "take identifier" (parse "import 'a.csv' take A" @?= AST [UnaliasedImport "a.csv"] "A" [] Nothing [])
        ],
        testGroup "join" 
        [
            testCase "no join" (parse "import A 'a.csv', B 'b.csv' take A" @?= AST [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "A" [] Nothing []),
            testCase "cross join" (parse "import A 'a.csv', B 'b.csv' take A cross join B" @?= AST [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "A" [Cross "B"] Nothing []),
            testCase "inner join with equals" (parse "import A 'a.csv', B 'b.csv' take A inner join B on A.1 = B.1" @?= AST [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "A" [Inner "B" (BinaryOpExpr (TableColumn "A" 1) AST.EQ (TableColumn "B" 1))] Nothing []),
            testCase "inner join with equals" (parse "import A 'a.csv', B 'b.csv', C 'c.csv' take A inner join B on A.1 = B.1, cross join C" @?= AST [AliasedImport "C" "c.csv", AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "A" [Cross "C", Inner "B" (BinaryOpExpr (TableColumn "A" 1) AST.EQ (TableColumn "B" 1))] Nothing [])
        ],
        testGroup "filter" 
        [
            testCase "unary operator" (parse "import A 'a.csv', B 'b.csv' take A cross join B where not 0" @?= AST [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "A" [Cross "B"] (Just (UnaryOpExpr NOT (ValueExpr (ValueInt 0)))) []),
            testCase "binary operator" (parse "import A 'a.csv', B 'b.csv' take A cross join B where a.1 = 0" @?= AST [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "A" [Cross "B"] (Just (BinaryOpExpr (TableColumn "a" 1) AST.EQ (ValueExpr (ValueInt 0)))) []),
            testCase "function" (parse "import A 'a.csv', B 'b.csv' take A cross join B where Coalesce(a.1,a.2)" @?= AST [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "A" [Cross "B"] (Just (Function "Coalesce" [TableColumn "a" 2,TableColumn "a" 1])) []),
            testCase "function and unary operator" (parse "import A 'a.csv', B 'b.csv' take A cross join B where Len(a.1) = 0" @?= AST [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "A" [Cross "B"] (Just (BinaryOpExpr (Function "Len" [TableColumn "a" 1]) AST.EQ (ValueExpr (ValueInt 0)))) [])
        ],
        testGroup "expression" [
            testCase "equality precedence" $ parseExpr "0+1=2" @?= "(0+1)=2"
        ]      
    ]

parse :: String -> Query
parse = parseJCQL . alexScanTokens

parseExpr :: String -> String
parseExpr s = (init . tail) $ printExpr $ getExpr ast
    where ast = parse ("import 'a.csv' take a where "++s)
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