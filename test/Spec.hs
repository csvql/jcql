import           InterpreterSpec                ( testInterpreter )
import           LexerSpec                      ( testLexer )
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import ParserSpec (testParser)
import IntegrationSpec (testIntegration)

main :: IO ()
main = defaultMain testAll

testAll :: TestTree
testAll = testGroup "Unit Tests" [testLexer, testInterpreter, testParser, testIntegration]
