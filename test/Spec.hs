import Test.Tasty (defaultMain, testGroup, TestTree)
import LexerSpec (testLexer)
import ParserSpec (testParser)

main :: IO ()
main = defaultMain testAll

testAll :: TestTree
testAll = testGroup "Unit Tests" [testLexer,testParser]