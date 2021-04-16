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

testParser :: TestTree
testParser =
  testGroup
    "Parser"
    [
        testGroup "imports" 
        [
            testCase "Parses unaliased import" (parse "import 'a.csv' take x" @?= AST [UnaliasedImport "a.csv"] "x" [] (ValueExpr (ValueBool True)) []),
            testCase "Parses aliased import" (parse "import A 'a.csv' take x" @?= AST [AliasedImport "A" "a.csv"] "x" [] (ValueExpr (ValueBool True)) []),
            testCase "Parses multiple unaliased imports" (parse "import 'a.csv', 'b.csv' take x" @?= AST [UnaliasedImport "b.csv", UnaliasedImport "a.csv"] "x" [] (ValueExpr (ValueBool True)) []),
            testCase "Parses multiple aliased imports" (parse "import A 'a.csv', B 'b.csv' take x" @?= AST [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"] "x" [] (ValueExpr (ValueBool True)) [])
        ],
        testGroup "take" 
        [
            testCase "take identifier" (parse "import 'a.csv' take A" @?= AST [UnaliasedImport "a.csv"] "A" [] (ValueExpr (ValueBool True)) [])
        ]
        

    ]

parse :: String -> Query
parse = parseJCQL . alexScanTokens