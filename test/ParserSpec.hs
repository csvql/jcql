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
        testCase "Parses import" (parse "import 'a.csv'" @?= AST [UnaliasedImport "a.csv"] [] [] (ValueExpr (ValueBool True)) [])
    ]




parse :: String -> Query
parse = parseJCQL . alexScanTokens